# Hash as an erlang process
##### Should Not Be Used Without Adult Supervision

[![Build Status](https://travis-ci.org/eiri/horrible-hash.svg?branch=master)](https://travis-ci.org/eiri/horrible-hash)
[![No Maintenance Intended](http://unmaintained.tech/badge.svg)](http://unmaintained.tech/)

## What is it
This is an erlang module that makes a mutable hash (in Perl's meaning) with a little help of process dictionary.


## Motivation
Mischief, mostly.


## Build

```bash
git clone https://github.com/eiri/horrible-hash.git
cd horrible-hash
make
make shell
```

```erlang
Erlang/OTP 18 [erts-7.2.1] [source] [64-bit] [smp:4:4] [async-threads:0] [hipe] [kernel-poll:false] [dtrace]

Eshell V7.2.1  (abort with ^G)
1> horrible_hash:new('$hash').
true
2> horrible_hash:set('$hash', key, value).
true
3> horrible_hash:exists('$hash', key).
true
4> horrible_hash:get('$hash', key).
value
5> horrible_hash:delete('$hash', key).
true
6> horrible_hash:exists('$hash', key).
false
7> horrible_hash:delete('$hash').
true
8> 
```


## Include in your project

Please don't.

Well, ok, if you insist.

```erlang
{deps, [
  {horrible_hash, {git, "https://github.com/eiri/horrible-hash.git"}, {tag, "0.1.0"}}
]}
```


## Usage

http://eiri.ca/horrible-hash/


## Random QA

*   _Should I use it in production?_<br />
    No.

*   _Yeah, but why not to use it?_<br />
    Because it destroys referencial transparency, makes debugging difficult and in general gives nothing ETS doesn't give.


## Licence

[MIT](https://github.com/eiri/horrible-hash/blob/master/LICENSE)
