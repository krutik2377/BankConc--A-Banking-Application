
% This bank file have all the functions related to bank such as generating processes , Pids , balance updater , bank amount management etc. which is kind of base file for banking related structure.  

-module(bank).
% Export
-export([create_processes_of_bank/1, request_handler/2,find_bank_record/1,create_bank_pids/1,start_listening/1,balance_updater/2,update_bank_record/1,bank_process/2]).

% Create processes for each bank
create_processes_of_bank(Banks) ->
    Pids = create_bank_pids(Banks),
    start_listening(Pids),
    Pids.

% Create individual bank processes and return their PIDs
create_bank_pids(Banks) ->
    lists:map(fun({BankName, Amount}) ->
        spawn(fun() -> bank_process(BankName, Amount) end)
    end, Banks).

% Update the balance of a bank
balance_updater(BankName, NewBalance) ->
    case find_bank_record(BankName) of
        {ok, {BankName, Amount, _}} ->
            UpdatedRecord = {BankName, Amount, NewBalance},
            update_bank_record(UpdatedRecord),
            ok;
        {error, not_found} ->
            io:format("Bank not found.~n"),
            error;
        {error, multiple_records} ->
            io:format("Multiple records found for the same BankName.~n"),
            error
    end.

% Find the bank record by its name
find_bank_record(BankName) ->
    case ets:lookup(bank_table, BankName) of
        [{BankName, Amount, Balance}] ->
            {ok, {BankName, Amount, Balance}};
        [] ->
            {error, not_found};
        _ ->
            {error, multiple_records}
    end.

% Update the bank record in the ETS table
update_bank_record(UpdatedRecord) ->
    ets:insert(bank_table, UpdatedRecord).

% Bank process
bank_process(BankName, Amount) ->
    receive
        {request, From, CustomerName, AmountNeeded} ->
            money:print_bank_request(CustomerName, AmountNeeded, BankName),
            % Handle the request and respond to the sender
            {Response, UpdatedAmount} = request_handler(AmountNeeded, Amount),
            money:print_bank_response(BankName, Response, AmountNeeded, CustomerName),
            balance_updater(BankName, UpdatedAmount),
            From ! {response, Response},
            bank_process(BankName, UpdatedAmount); % Continue listening for requests
        stop ->
            io:format("Stopping ~s process~n", [BankName]);
        _ ->
            bank_process(BankName, Amount) % Continue listening for requests
    end.

% Handle the customer request and determine the response
request_handler(AmountNeeded, CurrentAmount) ->
    case check_amount(AmountNeeded, CurrentAmount) of
        {approved, UpdatedAmount} ->
            {approved, UpdatedAmount};
    
        {rejected, InsufficientAmount} ->
            {rejected, InsufficientAmount}
    end.

% Check if the requested amount can be approved or rejected
check_amount(AmountNeeded, CurrentAmount) ->
    if
        AmountNeeded =< CurrentAmount ->
            UpdatedAmount = CurrentAmount - AmountNeeded,
            {approved, UpdatedAmount};
    
        true ->
            {rejected, CurrentAmount}
    end.

% Start listening for requests from customer processes
start_listening(Pids) ->
    lists:foreach(fun(Pid) -> Pid ! start_listening end, Pids).