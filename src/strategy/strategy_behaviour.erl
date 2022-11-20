-module(strategy_behaviour).

-callback nodes_call(Nodes_list :: list()) -> Nodes_data :: any().