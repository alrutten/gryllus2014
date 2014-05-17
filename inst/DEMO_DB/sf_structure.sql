CREATE TABLE file_status (
    
	id INTEGER PRIMARY KEY AUTOINCREMENT,
	path VARCHAR(255) NULL DEFAULT NULL,
	filesize INTEGER(10) NULL DEFAULT NULL, --COMMENT 'in KB',
	year_ INTEGER NULL DEFAULT NULL,
	date_ DATETIME NULL DEFAULT NULL, --COMMENT 'the date the SD-card got pulled (reliable date)',
	dt_last DATETIME NULL DEFAULT NULL,--COMMENT 'the last datetime in the file (unreliable for bad files)',
	feeder INTEGER(3) NULL DEFAULT NULL,
	lastBatdt DATETIME NULL DEFAULT NULL, --COMMENT 'date of the last BV in the file',
	upload_status INTEGER(2) NULL DEFAULT NULL, --COMMENT '0 = not uploaded, 1 = clean load, 2 = regexp load, 3 = manual load, -1 = load unsuccesful',
	datetime_valid FLOAT NULL DEFAULT NULL,-- COMMENT 'percentage of valid datetimes in the uploaded data (i.e., non-NA and within the time period that the datafile is from)',
	year_valid FLOAT NULL DEFAULT NULL,-- COMMENT 'percentage of valid year values (of non-NA datetimes)',
	n_timejumps INTEGER(11) NULL DEFAULT NULL,-- COMMENT 'number of backward timejumps in the file',
	lastRT FLOAT NULL DEFAULT NULL,-- COMMENT,-- 'last RTC battery voltage (<2.5 is bad, <1.8 is very bad)',
	lastBV FLOAT NULL DEFAULT NULL,-- COMMENT,-- 'last main battery voltage (<6 is bad, <5.8 is very bad)',
	remarks VARCHAR(255) NULL DEFAULT NULL,
	SD_problems VARCHAR(255) NULL DEFAULT NULL,
	dt_loaded DATETIME NULL DEFAULT NULL-- COMMENT 'when the data got transferred to the database',
	);
	
CREATE TABLE RAW_feeder (
	feeder INTEGER(3) NULL DEFAULT NULL,
	datetime_ DATETIME NULL DEFAULT NULL,
	PIR VARCHAR(2) NULL DEFAULT NULL,
	LB VARCHAR(2) NULL DEFAULT NULL,
	transp VARCHAR(16) NULL DEFAULT NULL,
	bv VARCHAR(7) NULL DEFAULT NULL,
	id INTEGER(10) NULL DEFAULT NULL,-- COMMENT 'id in the file status table',
	bout_length INTEGER(20) NULL DEFAULT NULL,
	r_pk INTEGER PRIMARY KEY AUTOINCREMENT
);	
	
--INDEXES

CREATE INDEX [fs.feeder] ON file_status ( 
    feeder ASC 
);
CREATE INDEX [RAW.feeder] ON RAW_feeder ( 
    feeder ASC 
);
CREATE INDEX [RAW.datetime_] ON RAW_feeder ( 
    datetime_ ASC 
);
CREATE INDEX [RAW.transp] ON RAW_feeder ( 
    transp ASC 
);