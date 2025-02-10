# Save to an S3 bucket.
library(paws)

s3 <- paws::s3()

# Need to have AWS keys set in env vars
# AWS_ACCESS_KEY_ID, AWS_SECRET_ACCESS_KEY, AWS_DEFAULT_REGION.

# Not buckets yet.
s3$list_buckets()

# Create one. Location constraint specification failed.
bucket_name <- 'arturo-bucket'
s3$create_bucket(
  ACL = "private",
  Bucket = bucket_name
  # CreateBucketConfiguration = list(LocationConstraint = "us-east-1")
)

# Upload the file
s3$put_object(
  Body = "app_data.Rdata",
  Bucket = bucket_name,
  Key = "app_data.Rdata"
)

# Did it work?
s3$list_objects(Bucket = bucket_name) |> dplyr::glimpse()
