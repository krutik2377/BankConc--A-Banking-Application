% This is controller file that controls the entire project by taking supporting function from customer and bank files.

-module(money).  
% Export
-export([start/1, print_bank_request/3, print_bank_response/4, generate_report/0]).

% Print a bank loan request message
print_bank_request(PersonName,Amount, BankName) ->
	io:format("? ~s requests a loan of ~p dollar(s) from the ~s bank.~n", [PersonName, Amount, BankName]).

% Print a bank loan response message
print_bank_response(BankName, Status, Amount, PersonName) ->
	io:format("$ The ~s bank ~s a loan of ~p dollar(s) to ~s.~n", [BankName, Status, Amount, PersonName]).

% to get banking report details
get_bank_report(TotalObjective, TotalReceived) ->
    BankData = ets:tab2list(bank_table),
    {TotalObjectiveAcc, TotalReceivedAcc} = process_bank_data(BankData, {TotalObjective, TotalReceived}),
    {TotalObjectiveAcc, TotalReceivedAcc}.

% Process bank data
process_bank_data([], {AccObj, AccRec}) ->
    {AccObj, AccRec};
process_bank_data([{BankName, Amount, Balance} | Rest], {AccObj, AccRec}) ->
    TotalObjectiveAcc = AccObj + Amount,
    TotalReceivedAcc = AccRec + Balance,
    io:format("~s: Total Amount is : ~p and Remaining Balance is : ~p~n", [BankName, Amount, Balance]),
    process_bank_data(Rest, {TotalObjectiveAcc, TotalReceivedAcc}).

% to get customer report details
get_customer_report(TotalRequested, TotalLoaned) ->
    CustomerData = ets:tab2list(customer_table),
    {TotalRequestedAcc, TotalLoanedAcc} = process_customer_data(CustomerData, {TotalRequested, TotalLoaned}),
    {TotalRequestedAcc, TotalLoanedAcc}.

% process data
process_customer_data([], {AccObj, AccRec}) ->
    {AccObj, AccRec};
process_customer_data([{CustomerName, Objective, Received} | Rest], {AccObj, AccRec}) ->
    TotalRequestedAcc = AccObj + Objective,
    TotalLoanedAcc = AccRec + Received,
    io:format("~s: Objective amount is : ~p and Received amount is : ~p~n", [CustomerName, Objective, Received]),
    process_customer_data(Rest, {TotalRequestedAcc, TotalLoanedAcc}).

% Generate final report 
generate_report() ->
	io:format("~n~n** Banking Report **~n~n"),
	
	
	io:format("------------------------------------------------------------~n"),
	io:format("|                    Customers details:                    |~n"),
    io:format("------------------------------------------------------------~n"),
	{TotalObjective, TotalReceived} = get_customer_report(0,0),
	io:format("----~n"),
	io:format("Total: objective amount: ~p, received amount: ~p~n", [TotalObjective, TotalReceived]),
	io:format("~n"),

	io:format("------------------------------------------------------------~n"),
	io:format("|                     Banks details:                       |~n"),
    io:format("------------------------------------------------------------~n"),
	{TotalRequested, TotalLoaned} = get_bank_report(0,0),
	io:format("----~n"),
	io:format("Total: original amount: ~p, loaned amount: ~p~n", [TotalRequested, TotalRequested-TotalLoaned]).

% updating ets table of customer details
create_customer_details_table(CustomerList) ->
    case ets:info(customer_table) of
        undefined ->
			ok;
		_ ->
           ets:delete(customer_table)
    end,
	
	Table = ets:new(customer_table, [named_table, set, public]),
    lists:foreach(
        fun({CustomerName, Amount}) ->
            Received = 0,
			Objective = Amount,
            Record = {CustomerName, Objective, Received},
            ets:insert(Table, Record)
        end,
        CustomerList
    ),
    Table.

% updating ets table of bank details
create_bank_details_table(BankList) ->
    case ets:info(bank_table) of
        undefined ->
			ok;
		_ ->
           ets:delete(bank_table)
    end,

	Table = ets:new(bank_table, [named_table, set, public]),
    lists:foreach(
        fun({BankName, Amount}) ->
            Balance = Amount,
            Record = {BankName, Amount, Balance},
            ets:insert(Table, Record)
        end,
        BankList
    ),
    Table.


% starting process of the program 
start(Args) ->
	io:format("** The financial market is opened for the day **~n"),
	timer:sleep(200),
	CustomerFile = lists:nth(1, Args),
	BankFile = lists:nth(2, Args),
	{ok, CustomerInfo} = file:consult(CustomerFile),
	{ok, BankInfo} = file:consult(BankFile),
	BankTable = create_bank_details_table(BankInfo),
	CustomerTable = create_customer_details_table(CustomerInfo),
	io:format("Starting transaction log...~n"),
	timer:sleep(200),
	BankPids = bank:create_processes_of_bank(BankInfo),
	customer:create_customer_processes(CustomerInfo, BankPids, BankTable, CustomerTable),
    io:format("~n~n** The financial market is closed for the day **~n").