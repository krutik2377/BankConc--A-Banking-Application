
% This file contains function related to customer such as creation of customer processes , generating report of customer , customer record management etc.

-module(customer).
% Exports
-export([create_customer_processes/4, get_random_bank_id/1, get_minimum_amount/1,create_customer_processes_spawn/5, generate_report/0,find_customer_record/1]).

% Create customer processes
create_customer_processes(Customers, BankPids, BankTable, CustomerTable) ->
    Parent = self(),
    CustomerPids = create_customer_processes_spawn(Customers, BankPids, Parent, BankTable, CustomerTable),
    completeCustomers(CustomerPids, length(CustomerPids)),
    generate_report().

% Spawn customer processes based on the provided list of customers
create_customer_processes_spawn([], _, _, _, _) ->
    [];
create_customer_processes_spawn([{CustomerName, Amount} | Rest], BankPids, Parent, BankTable, CustomerTable) ->
    CustomerPid = spawn(fun() -> customer_process(CustomerName, Amount, BankPids, Parent, BankTable, CustomerTable) end),
    [CustomerPid | create_customer_processes_spawn(Rest, BankPids, Parent, BankTable, CustomerTable)].

% Generate the report by calling the `generate_report` function from the `money` module
generate_report() ->
    money:generate_report().

% Wait for all customer processes to complete
completeCustomers(Processes, 0) ->
    Processes;
completeCustomers(Processes, Remaining) ->
    receive
        {process_completed, _} ->
            completeCustomers(Processes, Remaining - 1)
    end.

% Get the minimum amount between `Amount` and 50
get_minimum_amount(Amount) ->
    Minimum = case Amount =< 50 of
                   true -> rand:uniform(Amount);
                   false -> rand:uniform(50)
              end,
    Minimum.

% Get a random bank ID from the provided list of bank IDs
get_random_bank_id(BankIds) ->
    RandomIndex = generate_random_index(length(BankIds)),
    get_bank_id_at_index(RandomIndex, BankIds).

% Generate a random index within the specified range
generate_random_index(Max) ->
    rand:uniform(Max).

% Get the bank ID at the specified index in the list of bank IDs
get_bank_id_at_index(Index, BankIds) ->
    lists:nth(Index, BankIds).

% Generate a random wait time between 10 and 100
get_random_generated_wait_time() -> 
    WaitTime = rand:uniform(91), 
    WaitTime + 10. 

% Update the customer record with the received amount
customer_updater(CustomerName, Amount) ->
    case find_customer_record(CustomerName) of
        {ok, {CustomerName, Objective, Received}} ->
            NewReceived = Received + Amount,
            UpdatedRecord = {CustomerName, Objective, NewReceived},
            customer_updater_record(UpdatedRecord),
            ok;
        {error, not_found} ->
            io:format("Customer not found.~n"),
            error;
        {error, multiple_records} ->
            io:format("Multiple records found for the same CustomerName.~n"),
            error
    end.

% Find the customer record by customer name
find_customer_record(CustomerName) ->
    case ets:lookup(customer_table, CustomerName) of
        [{CustomerName, Objective, Received}] ->
            {ok, {CustomerName, Objective, Received}};
        [] ->
            {error, not_found};
        _ ->
            {error, multiple_records}
    end.

% Update the customer record in the ETS table
customer_updater_record(UpdatedRecord) ->
    ets:insert(customer_table, UpdatedRecord).


customer_process(CustomerName, Amount, BankPids, Parent, BankTable, CustomerTable) ->
    
    case {Amount, BankPids} of
        {0, _} ->
            Parent ! {process_completed, self()}; % Notify the parent process that the customer process is completed
        {_, []} ->
            Parent ! {process_completed, self()}; % Notify the parent process that the customer process is completed
        {_, _} ->
                BankPid = get_random_bank_id(BankPids), % Get a random bank ID from the available bank IDs
                AskingAmount = get_minimum_amount(Amount), % Get the minimum amount between `Amount` and 50
                BankPid ! {request, self(), CustomerName, AskingAmount}, % Send a request to the selected bank process
                receive
                    {response, Response} ->
                        case Response of
                            approved ->
                                timer:sleep(get_random_generated_wait_time()),
                                customer_updater(CustomerName, AskingAmount),
                                customer_process(CustomerName, Amount - AskingAmount, BankPids, Parent, BankTable, CustomerTable); % Recursively call the customer process with the remaining amount
                            _ ->
                                timer:sleep(get_random_generated_wait_time()),
                                customer_process(CustomerName, Amount, lists:delete(BankPid, BankPids), Parent, BankTable, CustomerTable) % Recursively call the customer process with the same amount and remove the selected bank ID from the list
                        end
                        
                end
            end.


