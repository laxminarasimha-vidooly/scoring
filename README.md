# scoring

Objective: To develop an algorithm inorder to predict the subscriber, last 30 days views and last 30 days engagement values of YT videos.

Business problem: Scoring is an algorithm to calculate the channel scores based on their subscribers, last 30 days views and engagement, last 90 days uploads.
This scoring is useful in understading channel performance. But, sometimes YT chennel owners hide their subscribers or last 30 days views and engagements 
are missing from tracking. 

Tools: R

Model used: Random forest

Process:

Lifetime subscribers prediction:

Training data was prepared from 1 lakh channels from daily tracking. video statistics were updated using YT API 

Attributes considered for prediction: Lifetime views, Lifetime uploads, Category, View_range and upload_range

30 days viewership prediction:

Training data was prepared from 1 lakh channels from daily tracking.video statistics were updated using YT API

Attributes considered for prediction: Lifetime subscribers, Lifetime views, Lifetime uploads, Category, View_range and upload_range, subscribers_range

30 days engagement prediction:

Training data was prepared from tracked users engagement data.

Attributes considered for prediction: Lifetime subscribers, Lifetime views, Lifetime uploads, Category, View_range and upload_range, subscribers_range

Test data:

Training data was split in to training and test data in 80:20 ratio for all models. Algorithm was tested against test data and prediction values 
were close enough to actual values.

