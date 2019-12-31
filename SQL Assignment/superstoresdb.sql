SET GLOBAL sql_mode = 'ONLY_FULL_GROUP_BY';
select @@GLOBAL.sql_mode;
-- Task 0 


-- Task 1 understanding data in hand

-- (A) Describe the data in hand in your own words. (Word Limit is 500).
/* The given database contains sales and order details of transactions done by customers in a superstore.
we are given 5 .csv files which contains the details as follows:-
cust_dimen :- contains details related to customer like name,id,region,segment 
prod_dimen :- contains details related to product_categories and sub_categories,product_ids
orders_dimen :- contains details related to order like number,date,priority
shipping_dimen :- contains details related to shipping mode,id,date,order_id
market_fact :- contains all ids like order_id,cust_id,prod_id,shipping_id,
also contains details of order quantity,discount,profit,shipping_cost,product_Base_Margin
 
 
 we can get many useful insights by joining these tables under some specified conditions.
 From the market_fact analysis,we can obtain various insights which help to analyze the segmentsand sales,
 Profitability of Products,shipping mode wise profitability.
 we can find the Regions where there are profits and Regions where we need to make improvements for attaining profits.
 we can also see the Products which have high order quantity,Low order quantity and find the reasons for 
 low order quantity products accordingly we can come up with a solution  
 which results in obtaining good profit by giving some offers or discounts that attracts the customers
 to buy the products which ultimately results in increase in the sales of the products besides making profits.
 
 */

-- (B) Identify and list the Primary and Foreign keys for this data set.
-- (Hint:- If a table doesn't have a primary or Foreign key then specifically mention it in your answer.

/* Now let's see Primary and Foreign keys in each table:-
1. cust_dimen :- Primary key :- Cust_id ; Foreign_key :- No Foreign key
2. prod_dimen :- Primary key :- Prod_id ; Foreign_Key :- No Foreign key
3. orders_dimen :- Primary key :- order_id ; Foreign Key :- No Foreign Key
4. shipping_dimen :- Primary key:- Shipping_id ; Foreign_Key :- order_ID 
5. market_fact :- Primary Key :- No Primary Key ; Foreign key :- Cust_id,Prod_id,order_id,ship_id  
*/


-- Task 2 :- Basic Analysis

-- click on the schema :- superstoresdb and set it as active schema

-- (A) Find the total and average sales (display total_sales,avg_sales)

select round(sum(sales),2) as 'Total_sales',round(avg(sales),2) as 'Average_sales'
from market_fact;

-- (B) Display the no.of customers in each region in decreasing order of no.of customers.
--  The result should contain columns Region and no. of customers.

select Region,count(Cust_id) as 'no_of_customers'
from cust_dimen
group by Region
order by count(Cust_id) desc; 


-- (c) Find the region having maximum customers (display region name and max(no.of customes).

select Region,count(Cust_id) as 'MAX(no_of_customers)'
from cust_dimen
group by Region
having count(Cust_id)=(select max(mycount) from
						(select Region,count(Cust_id) as mycount 
                        from cust_dimen group by Region)mytable);
                            
                            
-- (D) Find the number and id of products sold in descending order of products sold 
-- (display product_id,no.of products sold)

select Prod_id as 'Product ID',sum(Order_Quantity) as 'No_of_products_sold'
from  market_fact 
group by Prod_id
order by sum(Order_Quantity) desc;


-- (E) Find all the customers from Atlantic region who have ever purchased 'TABLES' and the number of tables purchased
--  (display customer_name and no_of_tables_purchased)

select Customer_Name as 'Customer Name',sum(Order_Quantity) as 'No_of_Tables_Purchased'
from cust_dimen c 
		inner join market_fact m on c.Cust_id=m.Cust_id 
		inner join prod_dimen p on m.Prod_id=p.Prod_id
where Region='Atlantic' and Product_Sub_Category='TABLES'
group by Customer_Name
order by sum(Order_Quantity) desc;



-- Task 3 :- Advanced Analysis

-- (A) Display the product categories in descending order of profits 
-- (display the product category wise profits,profits)?

select Product_Category as 'Product Category',round(sum(Profit),2) as 'Profits'
from prod_dimen p
		inner join market_fact m on p.Prod_id=m.Prod_id
group by Product_Category
order by sum(Profit) desc;


-- (B) Display the product category,product_sub_category and the profit with in each sub category in three columns

select Product_Category,Product_Sub_Category,round(sum(profit),2) as 'Profit'
from prod_dimen p
		inner join market_fact m on p.Prod_id=m.Prod_id
group by Product_sub_Category,Product_Category
order by Product_Category ;



-- (C) where is the least profitable product sub-category shipped the most? 
-- For the least profitable product_sub_category,display the region wise no_of_shipments
--  and the profit made in each region in descending order of profit
-- (i.e. region,no.of shipments,profit_in_each_region)
-- Note:-you can hardcore the name of the least profitable product sub category.


/*
select Product_Sub_Category as 'Least Profitable Product_Sub_Category',sum(Profit) as 'Profit'
from prod_dimen p 
		inner join market_fact m on p.Prod_id=m.Prod_id 
group by Product_Sub_Category
having sum(Profit)=(select min(myprofit) from
					 (select Product_Sub_Category,sum(Profit) as myprofit 
					  from prod_dimen p 
							inner join market_fact m on p.Prod_id=m.Prod_id
					  group by Product_Sub_Category)mytable);
Query to get Least Profitable Product_Sub_Category
*/


select Region,count(m.Ship_id) as 'No_of_shipments',round(sum(Profit),2) as 'Profit_in_each_region'
from market_fact m 
		inner join cust_dimen c on m.Cust_id=c.Cust_id 
        inner join prod_dimen p on m.Prod_id=p.Prod_id
where Product_Sub_Category = (select Product_Sub_Category as 'Least profitable Product_Sub_Category'
									from prod_dimen p 
											inner join market_fact m on p.Prod_id=m.Prod_id 
									group by Product_Sub_Category
									having sum(Profit)=(select min(myprofit) from
														(select Product_Sub_Category,sum(Profit) as myprofit 
														from prod_dimen p 
																inner join market_fact m on p.Prod_id=m.Prod_id 
														group by Product_Sub_Category)mytable))
group by Region
order by sum(Profit);
