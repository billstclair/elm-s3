[![elm-package](https://img.shields.io/badge/elm-1.0.0-blue.svg)](http://package.elm-lang.org/packages/billstclair/elm-s3/latest)

Pure-Elm client for [Amazon S3](https://aws.amazon.com/s3/) and [DigitalOcean Spaces](https://developers.digitalocean.com/documentation/spaces/) key/value stores. It targets a subset of the DigitalOcean subset of the Amazon S3 API.

# Example

The [`example`](https://github.com/billstclair/elm-s3/tree/master/example) directory has some sample code, with a simple user interface.

# Cross-Origin Resource Sharing (CORS)

It's very easy to get CORS errors when doing S3 requests from a web browser. You can't list buckets at all, and you need to set the CORS properties on buckets, with some tool other than a browser, in order to be able to access them from a browser.

[cors.xml](https://github.com/billstclair/elm-s3/tree/master/cors.xml) is an example of XML to send as the body in an [s3cmd](http://s3tools.org/s3cmd) "setcors" command:

    s3cmd setcors cors.xml s3://BUCKET
    
For Amazon S3, you can use the AWS Management Console to set the CORS properties. For Digital Ocean Spaces, you need to do it with an SDK or s3cmd. The Amazon S3 CORS documentation is [here](http://docs.aws.amazon.com/AmazonS3/latest/dev/cors.html).

# Credits

My thanks to Kevin Tonon for his [ktonen/elm-aws-core](http://package.elm-lang.org/packages/ktonon/elm-aws-core/latest) package. Without it, I would likely have thrown up my hands in despair over ever getting the signing and authorization crypto to work.
