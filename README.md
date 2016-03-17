# Hash as an erlang process
##### Should Not Be Used Without Adult Supervision

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
1> %% TBD
```

## Include in your project

Please don't.

Well, ok, if you insist.

```erlang
{deps, [
  {'horrible-hash', {git, "https://github.com/eiri/horrible-hash.git"}, {tag, "0.0.1"}}
]}
```

## Usage

:book: TBD

## Random QA

*   _Should I use it in production?_<br />
    No.

*   _You have a dash in the module's name, it makes it hard to use._<br />
    That's the point

## Licence

[MIT](https://github.com/eiri/horrible-hash/blob/master/LICENSE)
