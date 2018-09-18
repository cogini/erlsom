%%% @copyright 2006 - 2008 Willem de Jong
%%%
%%% This file is part of Erlsom.
%%%
%%% Erlsom is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU Lesser General Public License as
%%% published by the Free Software Foundation, either version 3 of
%%% the License, or (at your option) any later version.
%%%
%%% Erlsom is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU Lesser General Public License for more details.
%%%
%%% You should have received a copy of the GNU Lesser General Public
%%% License along with Erlsom.  If not, see
%%% <http://www.gnu.org/licenses/>.
%%%
%%% @author Willem de Jong <w.a.de.jong@gmail.com>

%%% ====================================================================
%%% Support for XML Schema in Erlang
%%% ====================================================================

%%% @doc This is the user interface for the Erlsom functions.

-module(erlsom).

%% user interface
-export([compile/1, compile/2, compile/3,
         compile_file/1, compile_file/2, compile_file/3,
         compile_xsd/1, compile_xsd/2, compile_xsd_file/1, compile_xsd_file/2,
         parse/2, parse_file/2,
         simple_form/1, simple_form/2,
         simple_form_file/1, simple_form_file/2,
         scan/2, scan/3, scan_file/2, scan_file/3,
         write/2, write/3,
         parse_sax/3, parse_sax/4,
         sax/3, sax/4,
         write_hrl/2, write_hrl/3,
         write_xsd_hrl_file/2, write_xsd_hrl_file/3,
         write_hrl_file/2, write_hrl_file/3, write_hrl_file/4,
         add_xsd_file/3,
         add_file/3, add_xsd_model/1, add_model/2]).

-include("erlsom.hrl").
-include("erlsom_parse.hrl").

-type characters() :: string() | binary().
-type prefix() :: string().
-type uri() :: string().
-type local_name() :: string().
-type attribute() :: erlsom_sax:attribute().

-type model() :: #model{}.
-export_type([model/0]).
-type sax_event() :: startDocument | endDocument |
  {startPrefixMapping, prefix(), uri()} | {endPrefixMapping, prefix()} |
  {startElement, uri(), local_name(), prefix(), [attribute()]} |
  {endElement, uri(), local_name(), prefix()} |
  {characters, characters()} |
  {ignorableWhitespace, characters()} |
  {error, any()} |
  {internalError, any()}.
-export_type([sax_event/0]).

-type event_fun() :: fun((sax_event(), term()) -> term()).
-export_type([event_fun/0]).

-type compile_option() :: {namespaces, [#ns{} | {Uri::string(), Prefix::string()}]} |
  {prefix, string()} |
  {type_prefix, string()} |
  {group_prefix, string()} |
  {element_prefix, string()} |
  % {include_fun, TODO} |
  {include_dirs, list(file:filename_all())} |
  {dir_list, list(file:filename_all())} |
  {include_any_attribs, boolean()} |
  % {value_fun, TODO} |
  % {include_files, TODO} |
  {strict, boolean()} |
  {already_imported, list({Uri::string(), Prefix::string()})}.
-export_type([compile_option/0]).

-type xml_data() :: string() | binary().
-export_type([xml_data/0]).

%% Error reason from file:read_file/1 or file:write_file/2
-type file_error() :: file:posix() | badarg | terminated | system_limit.

%% @doc Compile an XSD into a structure to be used by {@link scan/2}.
%% @param XSD the XSD.
%% @param Options [Option]
%%     Option = {prefix, Prefix} |
%%              {strict, boolean()} |
%%              {include_fun, Include_fun} |
%%              {include_dirs, Include_dirs} |
%%              {include_files, Include_list}
%%
%%     'Prefix' is prefixed to the record names in the XSD. It should be
%%        be a string or 'undefined'. If it is 'undefined', no prefix
%%        will be applied. The default is 'undefined'.
%%
%%     'Include_fun' is a function that finds the files that are
%%        included or imported in the XSD. It should be a function that
%%        takes 4 arguments:
%%           Namespace (from the XSD). This is a string or 'undefined'
%%           SchemaLocation (from the XSD). This is a string or 'undefined'
%%           Include_files. This is the value of the ‘include_files’ option if this
%%             option was passed to compile_xsd(); [] otherwise.
%%           Include_Dirs. This is the value of the Include_dirs option if provided,
%%             'undefined' otherwise.
%%           Prefix_list
%%        Include_fun should return the {XSD, Prefix}, where XSD is a
%%           XSD = string()
%%           Prefix = string or 'undefined', see above.
%%        Include_fun defaults to a function that searches the directories
%%        in Include_dirs for a file with the name SchemaLocation; it returns
%%        'undefined' for the prefix, unless this was specified in the
%%        prefix_list.
%%
%%     'Include_dirs' is a list of directories (strings), separated by commas.
%%       It defaults to ["."].
%%
%%     'Include_files' is a list of tuples {Namespace, Prefix, Location}.
%%
%%      'strict' - this enforces type checking of a number of additional
%%         types. If strict is false, the following types are checked and
%%         converted:
%%         - integer - converted to and from erlang integer
%%         - boolean - converted to an from an erlang boolean
%%         - qname   - converted to and from a #qname{} record
%%         All other types are treated as strings.
%%
%%         If strict is true, additionally the
%%         following types are checked and converted:
%%         - positiveInteger, ..TODO - all translated to integer
%%         - float - translated to/from a float or the atoms 'NaN',
%%           ..TODO
%%
%% Behaviour for includes:
%%
%% If 'include_fun' option is present, this function will be called. This should
%% return both the prefix and the file.
%%
%% Otherwise, if the 'includes' option is present, the list provided with this
%% option will be searched for a matching namespace. If this is found, the
%% specified prefix will be used. If a file is also specified, then this file will
%% be used. If no file is specified (value is undefined), then the 'location'
%% attribute and the 'include_dirs' option will be used to locate the file.
%%
%% If the 'includes' option is not present, or if the namespace is not found, then
%% the file will be searched for in the include_dirs (based on the 'location'
%% attribute). No prefix will be used.
%%
%% Returns: {ok, Model}, where Model is the internal structure, see
%% xml2struct.erl
%%----------------------------------------------------------------------
-spec compile_xsd(xml_data()) -> {ok, model()} | {error, term()}.
compile_xsd(Xsd) ->
  compile_xsd(Xsd, []).

-spec compile_xsd(xml_data(), [compile_option()]) -> {ok, model()} | {error, term()}.
compile_xsd(Xsd, Options) ->
  case catch erlsom_compile:compile(Xsd, Options) of
    {error, Message} -> {error, Message};
    {'EXIT', Message} -> throw({'EXIT', Message});
    Result -> Result
  end.

-spec compile_xsd_file(file:name_all()) -> {ok, model()} | {error, Reason} when
    Reason :: file_error().
compile_xsd_file(Xsd) ->
  compile_xsd_file(Xsd, []).

-spec compile_xsd_file(file:name_all(), [compile_option()]) -> {ok, model()} | {error, Reason} when
    Reason :: file_error().
compile_xsd_file(XsdFile, Options) ->
  case file:read_file(XsdFile) of
    {ok, Bin} ->
      compile_xsd(Bin, Options);
    Error ->
      Error
  end.

%% @deprecated Use {@link compile_xsd/2} instead.
%% @doc Compile XSD into a structure to be used by {@link parse/2}.
%% @param XSD the XSD.
%% @prefix Prefix String added to the record names in the XSD.
%% @prefix Namespaces List of [#ns], should include the URIs of imported namespaces;
%%     the purpose is to define the prefix for those.
%% @returns {ok, Model}, where Model is the internal structure, see
%% xml2struct.erl
%%----------------------------------------------------------------------
-spec compile(binary()) -> {ok, model()} | {error, term()}.
compile(Xsd) ->
  compile_xsd(Xsd, [{prefix, "p"}]).

-spec compile(binary(), string()) -> {ok, model()} | {error, term()}.
compile(Xsd, Prefix) ->
  compile_xsd(Xsd, [{prefix, Prefix}]).

-spec compile(binary(), string(), [#ns{}]) -> {ok, model()} | {error, term()}.
compile(Xsd, Prefix, Namespaces) ->
  compile_xsd(Xsd, [{prefix, Prefix}, {namespaces, Namespaces}]).

-spec compile_file(file:name_all()) -> model() | {error, term()}.
compile_file(XsdFile) ->
  compile_file(XsdFile, "p").

-spec compile_file(file:name_all(), string()) -> {ok, model()} | {error, term()}.
compile_file(XsdFile, Prefix) ->
  compile_file(XsdFile, Prefix, []).

-spec compile_file(file:name_all(), string(), [#ns{}]) -> {ok, model()} | {error, term()}.
compile_file(XsdFile, Prefix, Namespaces) ->
  case file:read_file(XsdFile) of
    {ok, Bin} ->
      compile(Bin, Prefix, Namespaces);
    Error ->
      Error
  end.

%%----------------------------------------------------------------------
%% @doc Translate an XML document that conforms to the XSD to a structure of records.
%%
%% The XSD is represented by {@link model()}, the result of the translation of the
%% XSD by {@link compile_xsd/1}.
%%
%% @param Xml the XML document.
%% @param Model the internal representation of the XSD; the result of {@link compile_xsd/1}.
%% @param Options
%%     Option = {continuation_function, Continuation_function,
%%               Continuation_state}
%%
%% If specified, the continuation function is called whenever the end of
%% the input XML document is reached before the parsing of the XML has finished.
%% The function should have 1 argument (Continuation_state). It should return
%% a tuple {NewData, NewState}, where NewData should be the next block of
%% data (a list of unicode code points), and NewState is the information that
%% is passed to the next invocation.
%%
%% Returns: {ok, Structure, TrailingCharacters}, where
%%     Structure is a structure of records that represent the XML
%%     document. See the documentation for the mapping;
%%     TrailingCharacters = any characters in the input string after the
%%       XML document.
%%----------------------------------------------------------------------
-spec scan(xml_data(), model()) -> {ok, term(), string()} | {error, term()}.
scan(Xml, Model) ->
  scan(Xml, Model, []).

-spec scan(xml_data(), model(), list()) -> {ok, term(), string()} | {error, term()}.
scan(Xml, #model{value_fun = ValFun} = Model, Options) ->
  State = #state{model=Model, namespaces=[], value_fun = ValFun},
  case lists:keysearch(acc, 1, Options) of
    {value, {_, Acc}} ->
      scan2(Xml, State#state{value_acc = Acc},
            lists:keydelete(acc, 1, Options));
    false ->
      scan2(Xml, State, Options)
  end.

%%cFunctionWrapper(T, S = #state{continuationState = {F, CS}}) ->
  %%{Data, CS2} = F(CS),
  %%{T ++ Data, S#state{continuationState = {F, CS2}}}.

-spec scan2(xml_data(), term(), list()) -> {ok, term(), string()} | {error, term()}.
scan2(Xml, State, Options) ->
  case catch erlsom_sax:parseDocument(Xml, State,
                                      fun erlsom_parse:xml2StructCallback/2,
                                      Options) of
    {error, Message} -> {error, Message};
    {'EXIT', Message} -> throw({'EXIT', Message});
    {ok, Structure, Tail} -> {ok, Structure, Tail}
  end.


-spec scan_file(file:name_all(), model()) -> {ok, term(), string()} | {error, term()}.
scan_file(File, Model) ->
  scan_file(File, Model, []).

-spec scan_file(file:name_all(), model(), list()) -> {ok, term(), string()} | {error, term()}.
scan_file(File, Model, Options) ->
  case file:read_file(File) of
    {ok, Bin} ->
      try
        scan(Bin, Model, Options)
      catch
        throw:Reason -> {error, Reason};
        exit:Reason -> throw({'EXIT',Reason});
        error:Reason -> throw({'EXIT',{Reason,erlang:get_stacktrace()}})
      end;
    Error ->
      Error
  end.

%% @deprecated Use {@link scan/2} instead.
%% @doc Same as {@link scan/2}, but without the trailing characters. If there are any
%% trailing characters they are ignored.
-spec parse(xml_data(), model()) -> {ok, term()} | {error, term()}.
parse(Xml, Model) ->
  case scan(Xml, Model) of
    {error, Message} -> {error, Message};
    {ok, Structure, _Tail} -> {ok, Structure}
  end.

parse_file(File, Model) ->
  case file:read_file(File) of
    {ok, Bin} ->
      parse(erlsom_lib:toUnicode(Bin), Model);
    Error ->
      Error
  end.

% -spec simple_form(Xml :: binary() | string(), [Option]) -> TODO when
% Option :: {nameFun, TODO}
% | {output_encoding, Encoding::TODO}

%% @doc Translate an XML document to 'simple form'.
%%
%% @param Xml an XML document (encoded binary or list or string())
%% @param Options
%%
%%     Option: {nameFun, NameFun} |
%%             {output_encoding, Encoding}
%%
%%  Namefun is a function with 3 arguments: Name, Namespace, Prefix.
%%  It should return a term. It is called for each tag and antribute
%%  name. The result will be used in the output. Default is Name
%%  if Namespace == undefined, and a string {Namespace}Name otherwise.
%%
%%  See parse_sax() for a description of the output_encoding option.
%%
%% Returns: {ok, SimpleForm, Tail}
%%     or {error, ErrorMessage}.
%%----------------------------------------------------------------------
-spec simple_form(xml_data(), list()) -> {ok, SimpleForm::term(), Tail::string()} | {error, Message::string()}.
simple_form(Xml, Options) ->
  erlsom_simple_form:scan(Xml, Options).

-spec simple_form(xml_data()) -> {ok, SimpleForm::term(), Tail::string()} | {error, Message::string()}.
simple_form(Xml) ->
  simple_form(Xml, []).

-spec simple_form_file(file:name_all()) -> {ok, SimpleForm::term(), Tail::string()} | {error, Message::string()}.
simple_form_file(File) ->
  simple_form_file(File, []).

-spec simple_form_file(file:name_all(), list()) -> {ok, SimpleForm::term(), Tail::string()} | {error, Message::string()}.
simple_form_file(File, Options) ->
  case file:read_file(File) of
    {ok, Bin} ->
      simple_form(Bin, Options);
    Error ->
      Error
  end.


%% @doc Translate a structure of records to an XML document.
%%
%% This is the inverse of erlsom:parse(). The XML will conform to an XSD, provided
%% that the input structure matches with this XSD.
%%
%% (The XSD is represented by the 'Model', the result of the translation
%% of the XSD by erlsom:compile().)
%%
%% @param Struct a structure or records that represents the XML document.
%% See the documentation for the mapping.
%%
%% @param Model the internal representation of the XSD; the result of
%%     erlsom:compile().
%%
%% @param Options: [{output, list | charlist | binary}]. In case the option 'list' is
%%     selected (this is the default), the output will be a list of unicode
%%     code points. In case the option 'charlist' is selected, the output will
%%     be a charlist, i.e. a deep lists of numbers representing Unicode
%%     code points and UTF-8 encoded binaries. If 'binary' is selected the
%%     output will be a UTF-8 encoded binary.
%%
%% Returns: {ok, Document} where Document is an XML document (a string),
%%     or {error, ErrorMessage}.
%%----------------------------------------------------------------------
-spec write(term(), model()) -> {ok, iodata()} | {error, Message::string()}.
write(Struct, Model) ->
  erlsom:write(Struct, Model, []).

-spec write(term(), model(), [Option]) -> {ok, iodata()} | {error, Message::string()} when
    Option :: {output, list | charlist | binary}.
write(Struct, Model, Options) ->
  erlsom_write:write(Struct, Model, Options).


%%----------------------------------------------------------------------
%% @doc parse an XML document using the org.xml.sax ContentHandler interface [SAX].
%% @param Xml A list of integers that correspond with the characters in an XML
%%         document. Can be either 1 byte characters or integers that
%%         correspond to Unicode code points.
%%
%% @param State a term() that is passed to the EventFun.
%%
%% @param  Eventfun - a fun() that is called by the parser whenever it has parsed
%%         a bit of the Xml input. The function is called by the parser
%%         according to the Sax specification (see [SAX]).
%%
%%         EventFun should accept the following arguments:
%%         - Event, a tuple that describes the event, see erlsom_sax for a
%%           description of the events
%%         - State - a term()
%%
%%         EventFun should return State, a term() that wil be passed back to
%%         the next invocation of EventFun.
%%
%% @param Options [Option]
%%     Option
%%       - {output_encoding, Encoding} This determines the encoding of
%%         the 'character data': element values and attribute values. The
%%         only supported encoding at this moment is 'utf8'. The default is
%%         string().
%%
%% @returns {ok, State, TrailingCharacters}
%%     State = the result of the last invocation of the callback function)
%%     TrailingCharacters = any characters in the input string after the
%%       XML document.
%%----------------------------------------------------------------------
-spec parse_sax(xml_data(), term(), event_fun(), list()) -> term().
parse_sax(Xml, State, EventFun, Options) ->
  erlsom_sax:parseDocument(Xml, State, EventFun, Options).

-spec parse_sax(xml_data(), term(), event_fun()) -> term().
parse_sax(Xml, State, EventFun) ->
  parse_sax(Xml, State, EventFun, []).

%%----------------------------------------------------------------------
%% Function: sax/3
%% Deprecated. Same as 'parse_sax/3', but without the trailing
%% characters. If there are any trailing characters they are ignored.
%%----------------------------------------------------------------------

sax(Xml, State, EventFun) ->
  sax(Xml, State, EventFun, []).

sax(Xml, State, EventFun, Options) ->
  {ok, Result, _TrailingCharacters} = parse_sax(Xml, State, EventFun, Options),
  Result.

-spec write_hrl(model(), file:name_all()) -> ok | {error, file_error()}.
write_hrl(Model, Output) ->
  write_hrl(Model, Output, []).

-spec write_hrl(model(), file:name_all(), list()) -> ok | {error, file_error()}.
write_hrl(Model, Output, Options) ->
  Hdr = erlsom_writeHrl:writeHrl(Model, Options),
  file:write_file(Output, Hdr).

%%----------------------------------------------------------------------
%% @doc Write record definitions (a .hrl file) for an XSD.
%% @param Xsd name of the input XSD file.
%% @param Output name of the output file.
%% @param Options Compile options plus the following:
%%
%%     'Namespaces' should include the URIs of all namespaces used
%%     in the XSD. 'Prefix' is prefixed to the record names.
%%
%%  * `{attribute_hrl_prefix, string()}' -- prefix for the record
%%  fields representing attributes. Defaults to "". E.g. if option
%%  {attribute_hrl_prefix, "attr_"} will be passed to this function,
%%  attribute "id" in the XML Schema will be represented by the field
%%  'attr_id' in the generated record. This is useful in the cases
%%  when a complex type have an attribute and an element with the
%%  same name.
%%
%% @returns ok or a file read/write error.
%%----------------------------------------------------------------------
-spec write_xsd_hrl_file(file:name_all(), file:name_all()) -> ok | {error, Reason} when
      Reason :: file_error().
write_xsd_hrl_file(Xsd, Output) ->
  write_xsd_hrl_file(Xsd, Output, []).

-spec write_xsd_hrl_file(file:name_all(), file:name_all(), list()) -> ok | {error, Reason} when
      Reason :: file_error().
write_xsd_hrl_file(Xsd, Output, Options) ->
  case file:read_file(Xsd) of
    {ok, Bin} ->
      Hdr = erlsom_writeHrl:writeXsdHrlFile(erlsom_lib:toUnicode(Bin), Options),
      file:write_file(Output, Hdr);
    Error ->
      Error
  end.

-spec write_hrl_file(file:name_all(), file:name_all()) -> ok | {error, Reason} when
      Reason :: file_error().
write_hrl_file(Xsd, Output) ->
  write_hrl_file(Xsd, "p", [], Output).

-spec write_hrl_file(file:name_all(), string(), file:name_all()) -> ok | {error, Reason} when
      Reason :: file_error().
write_hrl_file(Xsd, Prefix, Output) ->
  write_hrl_file(Xsd, Prefix, [], Output).

-spec write_hrl_file(file:name_all(), string(), [#ns{}], file:name_all()) -> ok | {error, Reason} when
    Reason :: file_error().
write_hrl_file(Xsd, Prefix, Namespaces, Output) ->
  case file:read_file(Xsd) of
    {ok, Bin} ->
      Hdr = erlsom_writeHrl:writeHrlFile(erlsom_lib:toUnicode(Bin), Prefix, Namespaces),
      file:write_file(Output, Hdr);
    Error ->
      Error
  end.

-spec add_xsd_file(file:name_all(), list(), model()) -> {ok, model()} | {error, Reason} when
      Reason :: file_error().
add_xsd_file(XsdFile, Options, Model) ->
  case file:read_file(XsdFile) of
    {ok, Bin} ->
      {ok, erlsom_add:add(erlsom_lib:toUnicode(Bin), Options, Model)};
    Error ->
      Error
  end.

-spec add_file(file:name_all(), string(), model()) -> model() | Reason when
    Reason :: file_error().
add_file(XsdFile, Prefix, Model) ->
  {_, Result} = add_xsd_file(XsdFile, [{prefix, Prefix}], Model),
  Result.

%% @doc Add the model for XML schema to a model.
%% We need a special function, since:
%% A - Erlsom can't parse the schema for XML schema
%% B - even if it would be able to parse the schema for XML schema,
%%     the output would be difficult to process.
%%
%% Expected to be used for parsing of WSDL files.
-spec add_xsd_model(model()) -> model().
add_xsd_model(Model) ->
  erlsom_add:add_xsd_model(Model).

%% @doc add Model2 to Model1
-spec add_model(erlsom:model(), erlsom:model()) -> erlsom:model().
add_model(Model1, Model2) ->
  erlsom_add:add_model(Model1, Model2).
