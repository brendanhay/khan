# Security Credentials

To test security credentials add a file named according to the desired
IAM profile with the following format:

```json
{
  "Code" : "Success",
  "LastUpdated" : "2013-09-06T08:52:21Z",
  "Type" : "AWS-HMAC",
  "AccessKeyId" : "access_key",
  "SecretAccessKey" : "secret_key",
  "Token" : "token",
  "Expiration" : "2013-09-06T14:57:30Z"
}
```

For example, to pass `--iam-profile s3_ro` as a global command line option this
directory should contain a file named `s3_ro` and `./metadata/server.rb` should
be started.
