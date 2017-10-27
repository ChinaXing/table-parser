CREATE TABLE `t_pay_record` (
	  `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT COMMENT 'ID',
	  `currency` char(3) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NOT NULL DEFAULT 'CNY' COMMENT '币种',
	  `inner_order_id` varchar(50) NOT NULL DEFAULT '' COMMENT '内部订单号',
	  `payee_id` varchar(45) NOT NULL DEFAULT '' COMMENT '收款人id',
	  `gmt_update` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT '修改时间',
	  `ext` varchar(1024) NOT NULL DEFAULT '' COMMENT '扩展信息',
	  PRIMARY KEY (`id`),
	  KEY `idx_combine` (`pay_mode`,`channel_code`,`biz_partner`,`inner_order_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='支付记录'
