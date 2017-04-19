####
# 
# FinancialInstrument-package	Construct, manage and store contract specifications for trading
# 
# add.defined.by	Add a source to the defined.by field of an 'instrument'
# add.identifier	Add an identifier to an 'instrument'
# alltick2sec	Convert tick data to one-second data
# 
# bond	instrument class constructors
# bond_series	Constructors for series contracts
# buildBasket	Construct a price/level series for pre-defined multi-leg spread instrument
# buildHierarchy	Construct a hierarchy of instruments useful for aggregation
# buildRatio	construct price ratios of 2 instruments
# buildSpread	Construct a price/level series for pre-defined multi-leg spread instrument
# build_series_symbols	construct a series of symbols based on root symbol and suffix letters
# build_spread_symbols	build symbols for exchange guaranteed (calendar) spreads
# butterfly	synthetic instrument constructors
# 
# C2M	Month-to-Code and Code-to-Month
# CompareInstrumentFiles	Compare Instrument Files
# currencies	currency metadata to be used by 'load.instruments'
# currency	instrument class constructors
# 
# Denotionalize	Convert price series to/from notional value
# 
# exchange_rate	constructor for spot exchange rate instruments
# expires	extract the correct expires value from an 'instrument'
# 
# FinancialInstrument	Construct, manage and store contract specifications for trading
# find.instrument	Find the primary_ids of instruments that contain certain strings
# FindCommonInstrumentAttributes	Find attributes that more than one instrument have in common
# fn_SpreadBuilder	Calculate prices of a spread from 2 instruments.
# formatSpreadPrice	format the price of a synthetic instrument
# format_id	format an id
# fund	instrument class constructors
# future	instrument class constructors
# future_series	Constructors for series contracts
# 
#### getInstrument	Primary accessor function for getting objects of class 'instrument'
# getSymbols.FI	getSymbols method for loading data from split files
# guaranteed_spread	synthetic instrument constructors
# 
# ICS	synthetic instrument constructors
# ICS_root	synthetic instrument constructors
# instrument	instrument class constructors
# instrument.auto	Create an instrument based on name alone
# instrument.table	Create data.frame with attributes of all instruments
# instrument_attr	Add or change an attribute of an instrument
# is.currency	class test for object supposedly of type 'currency'
# is.currency.name	check each element of a character vector to see if it is either the primary_id or an identifier of a 'currency'
#### is.instrument	class test for object supposedly of type 'instrument'
# is.instrument.name	check each element of a character vector to see if it is either the primary_id or an identifier of an 'instrument'
# 
# load.instruments	load instrument metadata into the .instrument environment
# loadInstruments	Save and Load all instrument definitions
# ls_AUD	shows or removes instruments of given currency denomination(s)
# ls_bonds	List or Remove instrument objects
# ls_by_currency	shows or removes instruments of given currency denomination(s)
# ls_by_expiry	list or remove instruments by expiration date
# ls_CAD	shows or removes instruments of given currency denomination(s)
# ls_calls	List or Remove instrument objects
# ls_CHF	shows or removes instruments of given currency denomination(s)
# ls_currencies	List or Remove instrument objects
# ls_derivatives	List or Remove instrument objects
# ls_EUR	shows or removes instruments of given currency denomination(s)
# ls_exchange_rates	List or Remove instrument objects
# ls_expires	show unique expiration dates of instruments
# ls_expiries	show unique expiration dates of instruments
# ls_funds	List or Remove instrument objects
# ls_futures	List or Remove instrument objects
# ls_future_series	List or Remove instrument objects
# ls_FX	List or Remove instrument objects
# ls_GBP	shows or removes instruments of given currency denomination(s)
# ls_guaranteed_spreads	List or Remove instrument objects
# ls_HKD	shows or removes instruments of given currency denomination(s)
# ls_ICS	List or Remove instrument objects
# ls_ICS_roots	List or Remove instrument objects
#### ls_instruments	List or Remove instrument objects
# ls_instruments_by	Subset names of instruments
# ls_JPY	shows or removes instruments of given currency denomination(s)
# ls_non_currencies	List or Remove instrument objects
# ls_non_derivatives	List or Remove instrument objects
# ls_NZD	shows or removes instruments of given currency denomination(s)
# ls_options	List or Remove instrument objects
# ls_option_series	List or Remove instrument objects
# ls_puts	List or Remove instrument objects
# ls_SEK	shows or removes instruments of given currency denomination(s)
# ls_spreads	List or Remove instrument objects
# ls_stocks	List or Remove instrument objects
# ls_strikes	show strike prices of defined options
# ls_synthetics	List or Remove instrument objects
# ls_underlyings	show names of underlyings
# ls_USD	shows or removes instruments of given currency denomination(s)
# 
# M2C	Month-to-Code and Code-to-Month
# make_spread_id	Construct a primary_id for a 'spread' 'instrument' from the primary_ids of its members
# MC2N	coerce month_cycle to a numeric vector
# month_cycle2numeric	coerce month_cycle to a numeric vector
# 
# next.future_id	Get the primary_id of the next-to-expire (previously expiring) future_series instrument
# Notionalize	Convert price series to/from notional value
# 
# option	instrument class constructors
# option_series	Constructors for series contracts
# option_series.yahoo	constructor for series of options using yahoo data
# 
# parse_id	Parse a primary_id
# parse_suffix	parse a suffix_id
# prev.future_id	Get the primary_id of the next-to-expire (previously expiring) future_series instrument
# 
# redenominate	Redenominate (change the base of) an instrument
# reloadInstruments	Save and Load all instrument definitions
# rm_bonds	List or Remove instrument objects
# rm_by_currency	shows or removes instruments of given currency denomination(s)
# rm_by_expiry	list or remove instruments by expiration date
# rm_currencies	List or Remove instrument objects
# rm_derivatives	List or Remove instrument objects
# rm_exchange_rates	List or Remove instrument objects
# rm_funds	List or Remove instrument objects
# rm_futures	List or Remove instrument objects
# rm_future_series	List or Remove instrument objects
# rm_FX	List or Remove instrument objects
# rm_instruments	List or Remove instrument objects
# rm_non_derivatives	List or Remove instrument objects
# rm_options	List or Remove instrument objects
# rm_option_series	List or Remove instrument objects
# rm_spreads	List or Remove instrument objects
# rm_stocks	List or Remove instrument objects
# rm_synthetics	List or Remove instrument objects
# root_contracts	future metadata to be used by 'load.instruments'
# 
# saveInstruments	Save and Load all instrument definitions
# saveSymbols.common	Save data to disk
# saveSymbols.days	Save data to disk
# setSymbolLookup.FI	set quantmod-style SymbolLookup for instruments
# sort_ids	sort primary_ids of instruments
# spread	synthetic instrument constructors
# stock	instrument class constructors
# synthetic	synthetic instrument constructors
# synthetic.instrument	synthetic instrument constructors
# 
# to_secBATV	Convert tick data to one-second data
# 
# update_instruments.instrument	Update instruments with metadata from another instrument.
# update_instruments.iShares	update iShares and SPDR ETF metadata
# update_instruments.masterDATA	Update instrument metadata for ETFs
# update_instruments.md	Update instrument metadata for ETFs
# update_instruments.morningstar	Update instrument metadata for ETFs
# update_instruments.ms	Update instrument metadata for ETFs
# update_instruments.SPDR	update iShares and SPDR ETF metadata
# update_instruments.TTR	updates instrument metadata with data from yahoo
# update_instruments.yahoo	updates instrument metadata with data from yahoo
# 
# volep	generate endpoints for volume bars
# 
# .get_rate	get an exchange rate series
# .to_daily	Extract a single row from each day in an xts object


## 1. currency 
##  ++ ls_instruments