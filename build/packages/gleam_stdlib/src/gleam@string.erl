-module(gleam@string).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([is_empty/1, length/1, reverse/1, replace/3, lowercase/1, uppercase/1, compare/2, slice/3, crop/2, drop_end/2, drop_right/2, contains/2, starts_with/2, ends_with/2, split_once/2, append/2, concat/1, repeat/2, join/2, pad_start/3, pad_left/3, pad_end/3, pad_right/3, trim_start/1, trim_left/1, trim_end/1, trim/1, trim_right/1, pop_grapheme/1, drop_start/2, drop_left/2, to_graphemes/1, split/2, to_utf_codepoints/1, from_utf_codepoints/1, utf_codepoint/1, utf_codepoint_to_int/1, to_option/1, first/1, last/1, capitalise/1, inspect/1, byte_size/1]).
-export_type([direction/0]).

-type direction() :: leading | trailing.

-file("/Users/louis/src/gleam/stdlib/src/gleam/string.gleam", 23).
-spec is_empty(binary()) -> boolean().
is_empty(Str) ->
    Str =:= <<""/utf8>>.

-file("/Users/louis/src/gleam/stdlib/src/gleam/string.gleam", 51).
-spec length(binary()) -> integer().
length(String) ->
    string:length(String).

-file("/Users/louis/src/gleam/stdlib/src/gleam/string.gleam", 65).
-spec reverse(binary()) -> binary().
reverse(String) ->
    _pipe = String,
    _pipe@1 = gleam_stdlib:identity(_pipe),
    _pipe@2 = string:reverse(_pipe@1),
    unicode:characters_to_binary(_pipe@2).

-file("/Users/louis/src/gleam/stdlib/src/gleam/string.gleam", 86).
-spec replace(binary(), binary(), binary()) -> binary().
replace(String, Pattern, Substitute) ->
    _pipe = String,
    _pipe@1 = gleam_stdlib:identity(_pipe),
    _pipe@2 = gleam_stdlib:string_replace(_pipe@1, Pattern, Substitute),
    unicode:characters_to_binary(_pipe@2).

-file("/Users/louis/src/gleam/stdlib/src/gleam/string.gleam", 111).
-spec lowercase(binary()) -> binary().
lowercase(String) ->
    string:lowercase(String).

-file("/Users/louis/src/gleam/stdlib/src/gleam/string.gleam", 127).
-spec uppercase(binary()) -> binary().
uppercase(String) ->
    string:uppercase(String).

-file("/Users/louis/src/gleam/stdlib/src/gleam/string.gleam", 145).
-spec compare(binary(), binary()) -> gleam@order:order().
compare(A, B) ->
    case A =:= B of
        true ->
            eq;

        _ ->
            case gleam_stdlib:less_than(A, B) of
                true ->
                    lt;

                _ ->
                    gt
            end
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/string.gleam", 190).
-spec slice(binary(), integer(), integer()) -> binary().
slice(String, Idx, Len) ->
    case Len < 0 of
        true ->
            <<""/utf8>>;

        false ->
            case Idx < 0 of
                true ->
                    Translated_idx = string:length(String) + Idx,
                    case Translated_idx < 0 of
                        true ->
                            <<""/utf8>>;

                        false ->
                            gleam_stdlib:slice(String, Translated_idx, Len)
                    end;

                false ->
                    gleam_stdlib:slice(String, Idx, Len)
            end
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/string.gleam", 223).
-spec crop(binary(), binary()) -> binary().
crop(String, Substring) ->
    gleam_stdlib:crop_string(String, Substring).

-file("/Users/louis/src/gleam/stdlib/src/gleam/string.gleam", 282).
-spec drop_end(binary(), integer()) -> binary().
drop_end(String, Num_graphemes) ->
    case Num_graphemes < 0 of
        true ->
            String;

        false ->
            slice(String, 0, string:length(String) - Num_graphemes)
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/string.gleam", 269).
-spec drop_right(binary(), integer()) -> binary().
drop_right(String, Num_graphemes) ->
    drop_end(String, Num_graphemes).

-file("/Users/louis/src/gleam/stdlib/src/gleam/string.gleam", 310).
-spec contains(binary(), binary()) -> boolean().
contains(Haystack, Needle) ->
    gleam_stdlib:contains_string(Haystack, Needle).

-file("/Users/louis/src/gleam/stdlib/src/gleam/string.gleam", 323).
-spec starts_with(binary(), binary()) -> boolean().
starts_with(String, Prefix) ->
    gleam_stdlib:string_starts_with(String, Prefix).

-file("/Users/louis/src/gleam/stdlib/src/gleam/string.gleam", 336).
-spec ends_with(binary(), binary()) -> boolean().
ends_with(String, Suffix) ->
    gleam_stdlib:string_ends_with(String, Suffix).

-file("/Users/louis/src/gleam/stdlib/src/gleam/string.gleam", 375).
-spec split_once(binary(), binary()) -> {ok, {binary(), binary()}} |
    {error, nil}.
split_once(String, Substring) ->
    case string:split(String, Substring) of
        [First, Rest] ->
            {ok, {First, Rest}};

        _ ->
            {error, nil}
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/string.gleam", 401).
-spec append(binary(), binary()) -> binary().
append(First, Second) ->
    _pipe = First,
    _pipe@1 = gleam_stdlib:identity(_pipe),
    _pipe@2 = gleam@string_tree:append(_pipe@1, Second),
    unicode:characters_to_binary(_pipe@2).

-file("/Users/louis/src/gleam/stdlib/src/gleam/string.gleam", 421).
-spec concat(list(binary())) -> binary().
concat(Strings) ->
    _pipe = Strings,
    _pipe@1 = gleam_stdlib:identity(_pipe),
    unicode:characters_to_binary(_pipe@1).

-file("/Users/louis/src/gleam/stdlib/src/gleam/string.gleam", 442).
-spec repeat_loop(binary(), integer(), binary()) -> binary().
repeat_loop(String, Times, Acc) ->
    case Times =< 0 of
        true ->
            Acc;

        false ->
            repeat_loop(String, Times - 1, <<Acc/binary, String/binary>>)
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/string.gleam", 438).
-spec repeat(binary(), integer()) -> binary().
repeat(String, Times) ->
    repeat_loop(String, Times, <<""/utf8>>).

-file("/Users/louis/src/gleam/stdlib/src/gleam/string.gleam", 461).
-spec join(list(binary()), binary()) -> binary().
join(Strings, Separator) ->
    _pipe = Strings,
    _pipe@1 = gleam@list:intersperse(_pipe, Separator),
    concat(_pipe@1).

-file("/Users/louis/src/gleam/stdlib/src/gleam/string.gleam", 589).
-spec padding(integer(), binary()) -> binary().
padding(Size, Pad_string) ->
    Pad_string_length = string:length(Pad_string),
    Num_pads = case Pad_string_length of
        0 -> 0;
        Gleam@denominator -> Size div Gleam@denominator
    end,
    Extra = case Pad_string_length of
        0 -> 0;
        Gleam@denominator@1 -> Size rem Gleam@denominator@1
    end,
    <<(repeat(Pad_string, Num_pads))/binary,
        (slice(Pad_string, 0, Extra))/binary>>.

-file("/Users/louis/src/gleam/stdlib/src/gleam/string.gleam", 514).
-spec pad_start(binary(), integer(), binary()) -> binary().
pad_start(String, Desired_length, Pad_string) ->
    Current_length = string:length(String),
    To_pad_length = Desired_length - Current_length,
    case To_pad_length =< 0 of
        true ->
            String;

        false ->
            <<(padding(To_pad_length, Pad_string))/binary, String/binary>>
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/string.gleam", 487).
-spec pad_left(binary(), integer(), binary()) -> binary().
pad_left(String, Desired_length, Pad_string) ->
    pad_start(String, Desired_length, Pad_string).

-file("/Users/louis/src/gleam/stdlib/src/gleam/string.gleam", 575).
-spec pad_end(binary(), integer(), binary()) -> binary().
pad_end(String, Desired_length, Pad_string) ->
    Current_length = string:length(String),
    To_pad_length = Desired_length - Current_length,
    case To_pad_length =< 0 of
        true ->
            String;

        false ->
            <<String/binary, (padding(To_pad_length, Pad_string))/binary>>
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/string.gleam", 548).
-spec pad_right(binary(), integer(), binary()) -> binary().
pad_right(String, Desired_length, Pad_string) ->
    pad_end(String, Desired_length, Pad_string).

-file("/Users/louis/src/gleam/stdlib/src/gleam/string.gleam", 647).
-spec trim_start(binary()) -> binary().
trim_start(String) ->
    string:trim(String, leading).

-file("/Users/louis/src/gleam/stdlib/src/gleam/string.gleam", 633).
-spec trim_left(binary()) -> binary().
trim_left(String) ->
    trim_start(String).

-file("/Users/louis/src/gleam/stdlib/src/gleam/string.gleam", 675).
-spec trim_end(binary()) -> binary().
trim_end(String) ->
    string:trim(String, trailing).

-file("/Users/louis/src/gleam/stdlib/src/gleam/string.gleam", 611).
-spec trim(binary()) -> binary().
trim(String) ->
    _pipe = String,
    _pipe@1 = trim_start(_pipe),
    trim_end(_pipe@1).

-file("/Users/louis/src/gleam/stdlib/src/gleam/string.gleam", 661).
-spec trim_right(binary()) -> binary().
trim_right(String) ->
    trim_end(String).

-file("/Users/louis/src/gleam/stdlib/src/gleam/string.gleam", 700).
-spec pop_grapheme(binary()) -> {ok, {binary(), binary()}} | {error, nil}.
pop_grapheme(String) ->
    gleam_stdlib:string_pop_grapheme(String).

-file("/Users/louis/src/gleam/stdlib/src/gleam/string.gleam", 248).
-spec drop_start(binary(), integer()) -> binary().
drop_start(String, Num_graphemes) ->
    case Num_graphemes > 0 of
        false ->
            String;

        true ->
            case gleam_stdlib:string_pop_grapheme(String) of
                {ok, {_, String@1}} ->
                    drop_start(String@1, Num_graphemes - 1);

                {error, nil} ->
                    String
            end
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/string.gleam", 235).
-spec drop_left(binary(), integer()) -> binary().
drop_left(String, Num_graphemes) ->
    drop_start(String, Num_graphemes).

-file("/Users/louis/src/gleam/stdlib/src/gleam/string.gleam", 716).
-spec to_graphemes_loop(binary(), list(binary())) -> list(binary()).
to_graphemes_loop(String, Acc) ->
    case gleam_stdlib:string_pop_grapheme(String) of
        {ok, {Grapheme, Rest}} ->
            to_graphemes_loop(Rest, [Grapheme | Acc]);

        _ ->
            Acc
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/string.gleam", 711).
-spec to_graphemes(binary()) -> list(binary()).
to_graphemes(String) ->
    _pipe = to_graphemes_loop(String, []),
    lists:reverse(_pipe).

-file("/Users/louis/src/gleam/stdlib/src/gleam/string.gleam", 347).
-spec split(binary(), binary()) -> list(binary()).
split(X, Substring) ->
    case Substring of
        <<""/utf8>> ->
            to_graphemes(X);

        _ ->
            _pipe = X,
            _pipe@1 = gleam_stdlib:identity(_pipe),
            _pipe@2 = gleam@string_tree:split(_pipe@1, Substring),
            gleam@list:map(_pipe@2, fun unicode:characters_to_binary/1)
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/string.gleam", 763).
-spec to_utf_codepoints_loop(bitstring(), list(integer())) -> list(integer()).
to_utf_codepoints_loop(Bit_array, Acc) ->
    case Bit_array of
        <<First/utf8, Rest/binary>> ->
            to_utf_codepoints_loop(Rest, [First | Acc]);

        _ ->
            lists:reverse(Acc)
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/string.gleam", 758).
-spec do_to_utf_codepoints(binary()) -> list(integer()).
do_to_utf_codepoints(String) ->
    to_utf_codepoints_loop(<<String/binary>>, []).

-file("/Users/louis/src/gleam/stdlib/src/gleam/string.gleam", 753).
-spec to_utf_codepoints(binary()) -> list(integer()).
to_utf_codepoints(String) ->
    do_to_utf_codepoints(String).

-file("/Users/louis/src/gleam/stdlib/src/gleam/string.gleam", 803).
-spec from_utf_codepoints(list(integer())) -> binary().
from_utf_codepoints(Utf_codepoints) ->
    gleam_stdlib:utf_codepoint_list_to_string(Utf_codepoints).

-file("/Users/louis/src/gleam/stdlib/src/gleam/string.gleam", 809).
-spec utf_codepoint(integer()) -> {ok, integer()} | {error, nil}.
utf_codepoint(Value) ->
    case Value of
        I when I > 1114111 ->
            {error, nil};

        I@1 when (I@1 >= 55296) andalso (I@1 =< 57343) ->
            {error, nil};

        I@2 when I@2 < 0 ->
            {error, nil};

        I@3 ->
            {ok, gleam_stdlib:identity(I@3)}
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/string.gleam", 830).
-spec utf_codepoint_to_int(integer()) -> integer().
utf_codepoint_to_int(Cp) ->
    gleam_stdlib:identity(Cp).

-file("/Users/louis/src/gleam/stdlib/src/gleam/string.gleam", 847).
-spec to_option(binary()) -> gleam@option:option(binary()).
to_option(String) ->
    case String of
        <<""/utf8>> ->
            none;

        _ ->
            {some, String}
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/string.gleam", 870).
-spec first(binary()) -> {ok, binary()} | {error, nil}.
first(String) ->
    case gleam_stdlib:string_pop_grapheme(String) of
        {ok, {First, _}} ->
            {ok, First};

        {error, E} ->
            {error, E}
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/string.gleam", 893).
-spec last(binary()) -> {ok, binary()} | {error, nil}.
last(String) ->
    case gleam_stdlib:string_pop_grapheme(String) of
        {ok, {First, <<""/utf8>>}} ->
            {ok, First};

        {ok, {_, Rest}} ->
            {ok, slice(Rest, -1, 1)};

        {error, E} ->
            {error, E}
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/string.gleam", 911).
-spec capitalise(binary()) -> binary().
capitalise(String) ->
    case gleam_stdlib:string_pop_grapheme(String) of
        {ok, {First, Rest}} ->
            append(string:uppercase(First), string:lowercase(Rest));

        _ ->
            <<""/utf8>>
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/string.gleam", 920).
-spec inspect(any()) -> binary().
inspect(Term) ->
    _pipe = gleam_stdlib:inspect(Term),
    unicode:characters_to_binary(_pipe).

-file("/Users/louis/src/gleam/stdlib/src/gleam/string.gleam", 943).
-spec byte_size(binary()) -> integer().
byte_size(String) ->
    erlang:byte_size(String).
