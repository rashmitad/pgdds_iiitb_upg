-- TASK 2: Baic Analysis=====================================================================================================================
-- Query to find the total and the average sales
select sum(Sales) as total_sales, avg(Sales) as avg_sales from market_fact;

-- Display the number of customers in each region in decreasing order of no_of_customers.
select  count(distinct Cust_id) as no_of_customers, Region from cust_dimen group by region order by no_of_customers desc;

-- Find the region having maximum customers
-- Solution 1
select  count(distinct Cust_id) as no_of_customers, Region from cust_dimen group by region order by no_of_customers desc LIMIT 1;
-- Alternative Solution
select max(result.no_of_customers) as 'Max(no_of_customers)', result.Region from 
(
select  count(distinct Cust_id) as no_of_customers, 
Region from cust_dimen 
group by region 
order by no_of_customers desc 
)
as result ;

-- Find the number and id of products sold in decreasing order of products sold
select Prod_id,sum(Order_Quantity) as no_of_products_sold 
from market_fact group by Prod_id order by no_of_products_sold desc;

-- Find all the customers from Atlantic region who have ever purchased ‘TABLES’ and the number of tables purchased (display the customer name, no_of_tables purchased)
select count(*) as no_of_tables, cust_dimen.Customer_Name
from market_fact 
inner join cust_dimen on cust_dimen.Cust_id=market_fact.Cust_id 
inner join prod_dimen on market_fact.Prod_id=prod_dimen.Prod_id 
where prod_dimen.Product_Sub_Category='TABLES' 
and 
cust_dimen.Region='ATLANTIC' group by Customer_Name order by no_of_tables desc;

-- ================================================================================================================================================
-- TASK 3: Advance Analysis
-- 
-- Display the product categories in descending order of profits (display the product category wise profits i.e. product_category, profits)?
select Product_Category,Profit from market_fact mf inner join prod_dimen pd on mf.Prod_id=pd.Prod_id order by Profit desc;

-- Display the product category, product sub-category and the profit within each sub-category in three columns.
select Product_Category,Product_Sub_Category,sum(Profit) from market_fact mf inner join prod_dimen pd on mf.Prod_id=pd.Prod_id group by Product_Sub_Category;

-- Where is the least profitable product subcategory shipped the most? 
-- For the least profitable product sub-category, display the region-wise no_of_shipments 
-- and the profit made in each region in decreasing order of profits 
-- (i.e. region, no_of_shipments, profit_in_each_region)
--   Note: You can hardcode the name of the least profitable product sub-category
select * from (
select sum(sd.Order_ID) as no_of_shipments, pd.Product_Category, pd.Product_Sub_Category,cd.Region, sum(mf.Profit)as Total_Profit from market_fact mf 
inner join prod_dimen pd on mf.Prod_id=pd.Prod_id 
inner join cust_dimen cd on cd.Cust_id=mf.Cust_id 
inner join shipping_dimen sd on mf.Ship_id=sd.Ship_id
group by cd.Region,pd.Product_Category, pd.Product_Sub_Category order by Total_Profit )
as res where res.Product_Sub_Category = 'TABLES' order by res.no_of_shipments desc ;