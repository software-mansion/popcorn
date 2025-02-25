%% There is a possibility of ill-implemented single catch command that is why this patch is implemented.
-module(erl_parse).

-export([first_anno/1]).

first_anno(Abstract) ->
    Anno0 = element(2, Abstract),
    F = fun(Anno, Anno1) ->
                Loc = erl_anno:location(Anno),
                Loc1 = erl_anno:location(Anno1),
                case loc_lte(Loc, Loc1) of
                    true ->
                        Anno;
                    false ->
                        throw(Anno1)
                end
        end,
%%  ORIGINAL CODE:
%%  catch fold_anno(F, Anno0, Abstract).
	try erl_parse:fold_anno(F, Anno0, Abstract)
	catch
		Error -> Error
	end.

loc_lte(Line1, Location2) when is_integer(Line1) ->
  loc_lte({Line1, 1}, Location2);
loc_lte(Location1, Line2) when is_integer(Line2) ->
  loc_lte(Location1, {Line2, 1});
loc_lte(Location1, Location2) ->
  Location1 =< Location2.