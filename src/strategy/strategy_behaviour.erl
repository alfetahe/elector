-module(strategy_behaviour).

-type leader() :: node().

-callback elect() -> Leader :: leader().