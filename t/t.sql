CREATE TABLE SchemaAccount (
	  `ID`                          INT(10) UNSIGNED NOT NULL AUTO_INCREMENT,
	  Env                         VARBINARY(16) NOT NULL COMMENT '环境标识,包括PROD,QA,DAILY等环境',
	  `ClusterID`                   int unsigned nOT NULL,
	  `UserName`                    VARBINARY(64) NOT NULL,
	  `Password`                    VARBINARY(64) NOT NULL,
	  `Created`                     DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
	  `Updated`                     DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '更新时间',
	   PRIMARY Key (`ID`),
	   uNIQUE KEY idx_ClusterID_UserName (`ClusterID`, `UserName`)
) ENGINE=InnoDB AUTO_INCREMENT=0 DEFAULT CHARSET=utf8mb4;

