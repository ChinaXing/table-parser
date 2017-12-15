CREATE TABLE `goods_v2` (
 `id` int(10) UNSIGNED NOT NULL AUTO_INCREMENT COMMENT '页面ID',
 `alias` varchar(20) NOT NULL COMMENT '短地址',
 `kdt_id` int(10) UNSIGNED NOT NULL COMMENT '口袋通id',
 `third_user_id` int(10) UNSIGNED NOT NULL COMMENT '微信公众号ID',
 `title` varchar(100) NOT NULL COMMENT '标题',
 `price` int(10) UNSIGNED NOT NULL COMMENT '价格(分)',
 `origin` varchar(30) DEFAULT NULL,
 `postage` int(11) UNSIGNED NOT NULL DEFAULT '0' COMMENT '运费',
 `buy_url` varchar(1024) NOT NULL COMMENT '购买地址',
 `content` text CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL,
 `components` longtext NOT NULL COMMENT '自定义组件内容',
 `components_extra_id` int(11) NOT NULL DEFAULT '0' COMMENT '店铺详情额外组件',
 `created_time` datetime NOT NULL COMMENT '创建时间',
 `update_time` datetime NOT NULL COMMENT '最后更新时间',
 `num` int(10) UNSIGNED NOT NULL DEFAULT '0' COMMENT '序号',
 `is_display` tinyint(3) UNSIGNED NOT NULL DEFAULT '1' COMMENT '是否显示（上架） 0：未发布，草稿状态（wap看不到） 1：发布 （正常状态）',
 `is_delete` tinyint(3) UNSIGNED NOT NULL DEFAULT '0' COMMENT '是否标记删除 0：否 1：是',
 `goods_type` tinyint(4) UNSIGNED NOT NULL DEFAULT '0' COMMENT '出售方式：0 默认，1 拍卖',
 `sold_status` tinyint(4) UNSIGNED NOT NULL DEFAULT '0' COMMENT '1,发售中 2,已售罄 3,部分售罄',
 `star` tinyint(4) UNSIGNED NOT NULL DEFAULT '0',
 `remark` varchar(100) DEFAULT NULL,
 `buy_way` tinyint(1) UNSIGNED NOT NULL DEFAULT '0' COMMENT '购买方式（微信，微博）',
 `goods_no` varchar(50) DEFAULT NULL COMMENT '货号',
 `is_virtual` tinyint(2) UNSIGNED NOT NULL DEFAULT '0',
 `quota` int(10) UNSIGNED NOT NULL DEFAULT '0',
 `messages` text,
 `class1` int(11) NOT NULL DEFAULT '0' COMMENT '一级品类',
 `class2` varchar(20) NOT NULL DEFAULT '' COMMENT '二级品类',
 `is_lock` tinyint(4) NOT NULL DEFAULT '0' COMMENT '是否锁定 1:是;0:否; 2,活动冻结',
 `start_sold_time` int(11) UNSIGNED NOT NULL DEFAULT '0',
 `join_level_discount` tinyint(3) UNSIGNED NOT NULL DEFAULT '0' COMMENT '是否参加会员折扣，默认：0 不参加',
 `source` tinyint(3) UNSIGNED NOT NULL DEFAULT '0' COMMENT '0自建，1导入，2api',
 `out_id` varchar(128) DEFAULT NULL,
 `take_down_time` int(11) UNSIGNED NOT NULL DEFAULT '0' COMMENT '下架时间',
 `delivery_template_id` int(10) DEFAULT '0' COMMENT '运费模板ID',
 `goods_platform` tinyint(3) UNSIGNED NOT NULL DEFAULT '0' COMMENT '商品参与平台 10：分销平台',
 `tag` text COMMENT '商品分类（冗余）',
 `hide_stock` tinyint(3) DEFAULT NULL COMMENT '隐藏库存',
 `total_stock` int(10) DEFAULT NULL COMMENT '总库存',
 `video_id` bigint(20) UNSIGNED NOT NULL DEFAULT '0' COMMENT '主视频id',
 `picture` text COMMENT '图片',
 `picture_height` int(10) DEFAULT NULL COMMENT '图片高度',
 `sold_time` tinyint(3) DEFAULT NULL COMMENT '开卖时间',
 `total_id` int(10) DEFAULT NULL COMMENT '综合SKUID',
 `total_sold_num` int(10) DEFAULT NULL COMMENT '总销量',
 `total_price` int(10) DEFAULT NULL COMMENT 'SKU 最低价',
 `invoice` tinyint(3) DEFAULT NULL COMMENT '是否提供发票',
 `warranty` tinyint(3) DEFAULT NULL COMMENT '是否保修',
 `sub_title` varchar(100) DEFAULT NULL COMMENT '子标题',
 `supplier_kdt_id` int(11) UNSIGNED NOT NULL DEFAULT '0' COMMENT '供货商 KDT_ID',
 `supplier_goods_id` int(11) UNSIGNED NOT NULL DEFAULT '0' COMMENT '供货商商品 ID',
 `mark` tinyint(3) UNSIGNED NOT NULL DEFAULT '0',
 `freezing_endtime` datetime NOT NULL COMMENT '活动冻结结束时间',
 `snap_key` varchar(50) NOT NULL DEFAULT '' COMMENT '当前商品镜像的key',
 `status` bigint(20) UNSIGNED NOT NULL DEFAULT '0' COMMENT '状态位',
 `canal` tinyint(4) NOT NULL DEFAULT '0' COMMENT '渠道，0：网店，1：门店',
 PRIMARY KEY (`id`),
 UNIQUE `alias` (`alias`),
 KEY `kdt_delete_display` (`kdt_id`, `is_delete`, `is_display`),
 KEY `kdt_goods_type` (`kdt_id`, `goods_type`),
 KEY `kdt_buy_way` (`kdt_id`, `buy_way`, `is_delete`)
) ENGINE = InnoDB CHARSET = utf8mb4