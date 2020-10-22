# author: Gene
# 10/15/2020

# Clear all lists
rm(list = ls())

# Load required packages
library(dplyr)
library(arules)
library(tidyr)
library(ggplot2)

# Load in the datasets
cust = read_csv("assignment2/olist_customers_dataset.csv")
items = read_csv("assignment2/olist_order_items_dataset.csv")
orders = read_csv("assignment2/olist_orders_dataset.csv")
products = read_csv("assignment2/olist_products_dataset.csv")
trans = read_csv("assignment2/product_category_name_translation.csv")
names(trans)[1] = 'product_category_name'

# coffee = read.transactions("coffeeshop.csv", format = "basket",
#                            sep = ",", rm.duplicates = TRUE)

# anti_joins
# check <- orders %>% anti_join(items, by = "order_id") %>%
#   left_join(cust, by = "customer_id")
# 
# items %>% anti_join(orders, by = "order_id") %>%
#   nrow()
# 
# write.csv(check, "775_records_to_check.csv", row.names = F, na = "")

# Join items and products by product_id, keep order_id, 
# order_item_id, product_category_name
new_cust = cust %>% inner_join(orders, by='customer_id') %>% 
            inner_join(items, by='order_id')

tmp_new_cust = new_cust %>% select(customer_unique_id, order_id, order_item_id,
                                   product_id)

# items = items %>%
#   left_join(products, by = "product_id")

items = tmp_new_cust %>%
  left_join(products, by = "product_id")

# Join item with trans by product_category_name, keep order_id, 
# order_item_id, product_category_name_english
new_items = items %>%
  inner_join(trans, by = "product_category_name") %>% # casey's left_join
  select(order_id, customer_unique_id, product_category_name_english) %>% 
  arrange(customer_unique_id)


new_item_row_num = new_items %>% group_by(customer_unique_id) %>% 
  summarize(cust_unique_order_item_id = row_number())

final_new_items = new_items %>% bind_cols(new_item_row_num$cust_unique_order_item_id)
names(final_new_items)[4]<- 'cust_unique_order_item_id'

# write.csv(final_new_items[c(2,3)], "final_new_items.csv", row.names = F, na = "")
# order_item_id

# Reshape from long to wide by splitting order_item_id
items_wide = final_new_items %>% spread(key = cust_unique_order_item_id, value = product_category_name_english)

# write.csv(items_wide, "items_wide.csv", row.names = F, na = "")

items_wide = items_wide %>% select(-order_id, -customer_unique_id)

write.csv(items_wide, "items_wide.csv", row.names = F, na = "")

# items_wide  = read.csv('items_wide.csv')

# Test conversion to transactions type
# 
# dat = read.transactions('items_wide.csv',
#                         format = 'basket',
#                         header = T,
#                         sep = ',',
#                         rm.duplicates = T)
# 
# itemInfo(dat)
# itemFrequency(dat, type = 'absolute')
# itemFrequencyPlot(dat)

# Part 2

test <- new_items %>% group_by(customer_unique_id, product_category_name_english) %>% summarize(n = n())
test_2 <- test %>% spread(key = product_category_name_english, value = n) %>% replace(is.na(.), 0)
test_2 <- as.data.frame(test_2)

normalize = function(x){
  return ((x - min(x, na.rm = T))/(max(x, na.rm = T) - min(x,na.rm = T)))}

normFunc <- function(x){(x-mean(x, na.rm = T))/sd(x, na.rm = T)}

test_2_normalized = test_2 %>% mutate(across(colnames(test_2)[c(2:72)], normalize))

kcluster = kmeans(test_2_normalized[,2:72], centers = 3) # pretty clear on 2
kcluster = kmeans(test_2_normalized[,2:72], centers = 5) # pretty clear on 2
kcluster = kmeans(test_2_normalized[,2:72], centers = 7) # pretty clear on 5

# kcluster$centers
test_2_normalized$cluster <- kcluster$cluster

cust_pred_label <- cust %>% inner_join(test_2_normalized[c("customer_unique_id", 'cluster')],
                                       by="customer_unique_id")
count_cust_pred_label <- cust_pred_label %>% group_by(customer_state, cluster) %>%
                          summarize(count=n())

ggplot(count_cust_pred_label, aes(cluster, customer_state, fill= count)) + 
  geom_tile()

kc_df <- as.data.frame(kcluster$centers)
