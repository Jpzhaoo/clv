# 加载包 ----

library(tidymodels)
library(vip)
library(tidyverse)
library(timetk)
library(lubridate)

# 1.0 准备数据 ----

cdnow_raw_tbl <- vroom::vroom(
    file = "data/CDNOW_master.txt",
    delim = " ",
    col_names = FALSE
)

# * 数据整洁 -----
cdnow_tbl <- cdnow_raw_tbl %>%
    select(X2, X3, X5, X8) %>%
    set_names(
        c("customer_id", "date", "quantity", "price")
    ) %>%
    mutate(date = ymd(as.character(date))) %>%
    drop_na()


# 2.0 群体分析 ----
# - 仅仅考虑当天有购买行为的用户

# * 用户第一次购买的日期范围 ----
cdnow_first_purchase_tbl <- cdnow_tbl %>%
    group_by(customer_id) %>%
    slice_min(date) %>%
    ungroup()

cdnow_first_purchase_tbl %>%
    pull(date) %>%
    range()

# "1997-01-01" "1998-06-26" 时间跨度较长


#   - 设定首次购买的时间范围: 1997-01-01 1997-03-31

# 挑选出第一次购买在1997-01-01 到 1997-03-31时间段内的客户id
cdnow_first_purchase_tbl %>%
    subset(date >= "1997-01-01" & date <= "1997-03-31") %>%
    distinct(customer_id)

ids_in_cohort <- cdnow_first_purchase_tbl %>%
    filter_by_time(
        .start_date = "1997-01",
        .end_date   = "1997-03"
    ) %>%
    distinct(customer_id) %>%
    pull(customer_id)

# 筛选出第一次购买时间在1997-01-01到1997-03-31之间客户的所有购买记录
cdnow_cohort_tbl <- cdnow_tbl %>%
    filter(customer_id %in% ids_in_cohort)

# * 该群体总购买量 ----

cdnow_cohort_tbl %>%
    summarize_by_time(
        total_price = sum(price, na.rm = TRUE),
        .by   = "month"
    ) %>%
    plot_time_series(date, total_price, .y_intercept = 0,
                     .smooth = FALSE)
## 消费前十的用户
top_10_customers <- cdnow_cohort_tbl %>%
    group_by(customer_id) %>%
    summarise(total_consume = sum(price, na.rm = TRUE)) %>%
    arrange(desc(total_consume)) %>%
    slice(1:10) %>%
    pull(customer_id)

cdnow_cohort_tbl %>%
    filter(customer_id %in% top_10_customers,
           date <= "1997-12-31") %>%
    mutate(month = month(date)) %>%
    plot_time_series(.date_var = month, price, .facet_vars = customer_id, .smooth = FALSE)


# * 查看具体某个用户的购买情况 ----
n    <- 1:10
ids  <- unique(cdnow_cohort_tbl$customer_id)[n]

cdnow_cohort_tbl %>%
    filter(customer_id %in% ids) %>%
    plot_time_series(
        date, price,
        .y_intercept = 0,
        .smooth      = FALSE,
        .facet_vars = customer_id,
        .facet_ncol  = 2,
        .interactive = FALSE,
        .title = "Customer Purchase Behavior"
    ) +
    geom_point(color = "#2c3e50")


# 3.0 建模 ----
#  分析目标:
#  - 消费者未来90天的消费金额是多少？
#  - 消费者未来90天消费的概率是多少？

# 3.1 数据划分 (分两步) ----

# ** Step 1: 通过用户 ID 随机划分 ----

set.seed(123)
ids_train <- cdnow_cohort_tbl %>%
    pull(customer_id) %>%
    unique() %>%
    sample(size = round(0.8 * length(.))) %>%
    sort()

split_1_train_tbl <- cdnow_cohort_tbl %>%
    filter(customer_id %in% ids_train)

split_1_test_tbl  <- cdnow_cohort_tbl %>%
    filter(!customer_id %in% ids_train)

# ** Step 2: 时间划分 ----

# 预留出90天用作训练
splits_2_train <- time_series_split(
    split_1_train_tbl,
    assess     = "90 days",
    cumulative = TRUE
)

# splits_2_train %>%
#     tk_time_series_cv_plan() %>%
#     filter(.key == "testing") %>%
#     pull(date) %>%
#     range()

splits_2_train %>%
    tk_time_series_cv_plan() %>%
    plot_time_series_cv_plan(date, price)

splits_2_test <- time_series_split(
    split_1_test_tbl,
    assess     = "90 days",
    cumulative = TRUE
)

splits_2_test %>%
    tk_time_series_cv_plan() %>%
    plot_time_series_cv_plan(date, price)

# 3.2 Feature Engineering (RFM) ----

targets_train_tbl <- testing(splits_2_train) %>%
    group_by(customer_id) %>%
    summarise(
        spend_90_total = sum(price),
        spend_90_flag    = 1
    )

targets_test_tbl <- testing(splits_2_test) %>%
    group_by(customer_id) %>%
    summarise(
        spend_90_total = sum(price),
        spend_90_flag    = 1
    )

# ** 对训练数据进行特征构造 ----
#    - RFM: Recency, Frequency, Monetary

max_date_train <- training(splits_2_train) %>%
    pull(date) %>%
    max()

train_tbl <- training(splits_2_train) %>%
    group_by(customer_id) %>%
    summarise(
        recency   = (max(date) - max_date_train) / ddays(1),
        frequency = n(),
        price_sum   = sum(price, na.rm = TRUE),
        price_mean  = mean(price, na.rm = TRUE)
    ) %>%
    left_join(
        targets_train_tbl
    ) %>%
    replace_na(replace = list(
        spend_90_total = 0,
        spend_90_flag  = 0
    )
    ) %>%
    mutate(spend_90_flag = as.factor(spend_90_flag))

# ** 对测试集进行相同的操作 ----

test_tbl <- training(splits_2_test) %>%
    group_by(customer_id) %>%
    summarise(
        recency     = (max(date) - max_date_train) / ddays(1),
        frequency   = n(),
        price_sum   = sum(price, na.rm = TRUE),
        price_mean  = mean(price, na.rm = TRUE)
    ) %>%
    left_join(
        targets_test_tbl
    ) %>%
    replace_na(replace = list(
        spend_90_total = 0,
        spend_90_flag  = 0
    )
    ) %>%
    mutate(spend_90_flag = as.factor(spend_90_flag))

# 3.3 建模 ----

# **  1: 90天消费额度预测 ----
recipe_spend_total <- recipe(spend_90_total ~ ., data = train_tbl) %>%
    step_rm(spend_90_flag, customer_id)

# **  2: 90天消费概率预测 ----
recipe_spend_prob <- recipe(spend_90_flag ~ ., data = train_tbl) %>%
    step_rm(spend_90_total, customer_id)

recipe_spend_prob %>% prep() %>% juice() %>% glimpse()

summary(recipe_spend_prob)

wflw_spend_total_xgb <- workflow() %>%
    add_model(
        boost_tree(
            mode = "regression"
        ) %>%
            set_engine("xgboost")
    ) %>%
    add_recipe(recipe_spend_total) %>%
    fit(train_tbl)


wflw_spend_prob_xgb <- workflow() %>%
    add_model(
        boost_tree(
            mode = "classification"
        ) %>%
            set_engine("xgboost")
    ) %>%
    add_recipe(recipe_spend_prob) %>%
    fit(train_tbl)

# 3.5 测试集评估 ----


predictions_test_tbl <-  bind_cols(
    
    predict(wflw_spend_total_xgb, test_tbl) %>%
        rename(.pred_total = .pred),
    
    predict(wflw_spend_prob_xgb, test_tbl, type = "prob") %>%
        select(.pred_1) %>%
        rename(.pred_prob = .pred_1)
) %>%
    bind_cols(test_tbl) %>%
    select(starts_with(".pred"), starts_with("spend_"), everything())


predictions_test_tbl %>%
    yardstick::mae(spend_90_total, .pred_total)

predictions_test_tbl %>%
    yardstick::roc_auc(spend_90_flag, .pred_prob, event_level = "second")

predictions_test_tbl %>%
    yardstick::roc_curve(spend_90_flag, .pred_prob, event_level = "second")%>%
    autoplot()

# 3.6 特征重要性 ----

vip(wflw_spend_prob_xgb$fit$fit)


vip(wflw_spend_total_xgb$fit$fit)

# # 3.7 保存 ----
# 
# fs::dir_create("artifacts")
# 
# wflw_spend_prob_xgb %>% write_rds("artifacts/model_prob.rds")
# wflw_spend_total_xgb %>% write_rds("artifacts/model_spend.rds")
# 
# vi_model(wflw_spend_prob_xgb$fit$fit) %>% write_rds("artifacts/vi_prob.rds")
# vi_model(wflw_spend_total_xgb$fit$fit) %>% write_rds("artifacts/vi_spend.rds")
# 
# all_tbl <- bind_rows(train_tbl, test_tbl)
# predictions_all_tbl <- bind_cols(
#     predict(wflw_spend_total_xgb, all_tbl) %>%
#         rename(.pred_total = .pred),
#     predict(wflw_spend_prob_xgb, all_tbl, type = "prob") %>%
#         select(.pred_1) %>%
#         rename(.pred_prob = .pred_1)
# ) %>%
#     bind_cols(all_tbl) %>%
#     select(starts_with(".pred"), starts_with("spend_"), everything())
# 
# predictions_all_tbl %>% write_rds("artifacts/predictions_all_tbl.rds")


# 4.0 结果运用 ----

# **哪些用户未来90天消费概率最大? ----
#    - 或许可以根据概率以及用户过去购买的商品推荐新的产品给用户
predictions_test_tbl %>%
    arrange(desc(.pred_prob))

# ** 哪些用户最近有购买但是未来不太可能购买? ----
#    - 可以通过发放优惠券或者其他的活动（如拉新用户）刺激消费者购买
predictions_test_tbl %>%
    filter(
        recency    > -90,
        .pred_prob < 0.2
    ) %>%
    arrange(.pred_prob)

# ** 哪些用户是高消费群体，但是未来购买概率很低 ----
#    - 了解其为什么不愿意再购买的原因，如何提高该类用户的留存率
predictions_test_tbl %>%
    arrange(desc(.pred_total)) %>%
    filter(
        spend_90_total == 0
    )

predictions_test_tbl %>%
    filter(
        spend_90_total == 0,
        .pred_prob < 0.2
    )

predictions_test_tbl %>%
    filter(
        spend_90_flag == 1,
        .pred_prob > 0.2
    ) %>%
    arrange(.pred_prob)

predictions_test_tbl %>%
    filter(
        spend_90_flag == 0
    )



