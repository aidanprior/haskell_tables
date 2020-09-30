# tables
A tables generator for camp

The program requires multiple csv files as data input: "Aides.csv" : A single column row-list of aide names "Camper List.csv" : A single column row-list of camper names by age (or cabin) "Counselor Tables.csv" : A row-list of counselors at each table, with a seperate column for each counselor Aide spots are designated with 'Aide'

The program will generate a "Conflicts.csv" file if one does not yet exist, where each column will be a list of campers and counselors with the same last name. These lists may be added to, removed from, or even new lists added to

If the "Conflicts.csv" file already exists, then the program will use those conflicts to generate the tables and save it in a "Tables-Week#.tsv" file and a "Lookup-Week#.tsv"

The program requires one commandline argument, the week number to generate for, this only affect the resultant filenames. The program will create tables for the week and then save in the coresponding files.