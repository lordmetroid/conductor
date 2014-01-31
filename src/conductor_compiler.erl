-module(conductor_compiler).

-export([
	make_programs/1,
	make_models/1
	make_views/1,
	make_controllers/1
]).

make_programs(Path) ->
	
make_models(Path) ->
	
make_views(Path) ->
		
make_controllers(Path) ->
	

%% ----------------------------------------------------------------------------
% @spec module_id() -> UUID::string()
% @doc Compile half an erlang file into a random module
%% ----------------------------------------------------------------------------
module_id() ->
	uuid:uuid_to_string(uuid:get_v4()).

	erl_syntax:attribute(erl_syntax:atom(module), [
		erl_syntax:atom(module_id())
	]).
