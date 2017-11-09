The [billstclair/elm-s3](http://package.elm-lang.org/packages/billstclair/elm-s3/latest) package (link does not yet exist) provides an Elm client for the [Amazon S3](https://aws.amazon.com/s3/) and [DigitalOcean Spaces](https://developers.digitalocean.com/documentation/spaces/) object storage systems. It currently targets a subset of the DigitalOcean subset of the Amazon S3 API.

The `example` directory has some sample code, with a simple user interface.

In order to use it, you'll need to get a secret key and access key. You can get thoseon the [API Tokens](https://cloud.digitalocean.com/settings/api/tokens) page for DigitalOcean Spaces. There are choices for Amazon AWS, but you'll probably want to create keys from the [Identity and Access Management (IAM)](https://console.aws.amazon.com/iam/) console.

Kevin Tonon has already done a lot of the basic work for this in his unreleased [ktonen/elm-aws-core](https://github.com/ktonon/elm-aws-core) package, which I found while looking at the source for [ktonen/elm-crypto](http://package.elm-lang.org/packages/ktonon/elm-crypto/latest) (HMAC and SHA).

I haven't decided yet whether to use [crazymykl/ex-em-elm](http://package.elm-lang.org/packages/crazymykl/ex-em-elm/latest) or [eeue56/elm-xml](http://package.elm-lang.org/packages/eeue56/elm-xml/latest), though the latter will be my first try (Noah's got creds).

I won't add elm-s3 to the [Elm package repository](http://package.elm-lang.org) until Kevin Tonan adds [elm-aws-generate](https://github.com/ktonon/elm-aws-generate). Until then, elm-aws-generate is a [Git submodule](https://git-scm.com/docs/gitsubmodules) of elm-s3.

In order to clone the submodule, after you clone this repository, you need to do:

    cd .../elm-s3
    git submodule init
    git submodule update
