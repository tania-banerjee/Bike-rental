Comparison between Member vs Casual
This is the capstone project that I solved as a part of Google Data Analytics Course.

Data source: If you want to reproduce this work, you may use the data stored in spreadsheets from here: https://divvy-tripdata.s3.amazonaws.com/index.html . The data has been made available by Motivate International Inc. under this license - https://ride.divvybikes.com/data-license-agreement . I used the 12 months data from April 2020 to March 2021.

Vision: To convert the Casual riders to Member riders

Mission: The marketing team of the bike-rental company will come up with offers that would be targeted to the rider-type Casual. To achieve this a comparison needs to be made between the Casual vs Member to understand how the two groups use the bikes differently.

Goals: 
I have crunched around 3,400,000 rows of data to understand-
1. How the rental bikes are used by days of the week by the two groups.
2. How the variance of the bikes fluctuates according to months.
3. What is the mean riding duration for the two groups.
4. Is there a variance in direction taken for the two groups.

Pro Tip for further development: 
1. Try finding something at the intersection of rideduration and direction taken. This would give you the answer if those "roundtrips" are happening or just someone starting and ending their ride instantly(and if this means they are facing problems in using the service). 

2. I have run 4 loops to "fill in the blanks", this has made the running time really long. However, you may drop the loops and proceed as those just filled in 10,000 rows and those (string)observations were pretty much useless.

WARNING: Since this is my first stint with R, I did the mistake of knitting the entire code in a RMarkdown, as a result that HTML file now contains loads of elements from the console which were basically a part of my cleaning process.
 main
