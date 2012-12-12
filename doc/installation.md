# yamerl Installation

## tl;dr

1. Take the sources from Github and generate Autotools files:

  ```bash
  git clone 'https://github.com/yakaz/yamerl.git'
  cd yamerl/
  autoreconf -vif
  ```

2. Enter the sources directory and run the usual configure/make/make install:

  ```bash
  ./configure
  make
  make install
  ```

The default installation path is your Erlang's distribution libraries directory.

## Requirements

### Build dependencies

* autoconf 2.64 or later
* automake 1.11 or later
* awk (tested with FreeBSD's awk, mawk and gawk, should work with any flavor of Awk)
* make (tested with FreeBSD's make and GNU make, should work with any flavor of make)
* Erlang/OTP R14B02 or later

### Testsuite dependencies

* Erlang/OTP R14B02 or later
* [yamler](https://github.com/goertzenator/yamler) (optional)

### Runtime dependencies

* Erlang/OTP R14B02 or later

## Building

### Generating the Autotools files

> If you use a release tarball, you can skip this step.

You need to generate the Autotools files:
* after a fresh clone of the Git repository;
* each time you modify `configure.ac` or any `Makefile.am`

```bash
autoreconf -vif
```

### Configuring the build

#### Inside sources vs. outside sources

* The simplest method is to run the `configure` script from the sources directory:

  ```bash
  ./configure
  ```

* The **recommended method** is to run the `configure` script from a separate directory, in order to keep the sources directory clean:

  ```bash
  # Create and enter a separate directory.
  mkdir build-yamerl
  cd build-yamerl

  # Execute the configure script from this directory; all files are
  # created in this directory, not in the sources directory.
  /path/to/yamerl-sources/configure
  ```

#### Changing the install path

The default installation path is your Erlang's distribution libraries directory, as reported by `code:lib_dir()`. To install in a different directory (eg. because you do not have sufficient privileges), you can use the `--prefix` option:
```bash
.../configure --prefix=$HOME/my-erlang-apps
```

#### Using a non-default Erlang distribution

By default, the system Erlang distribution is used by querying `erl(1)` taken from the `$PATH`. You can specify another Erlang distribution:

* using the `--with-erlang` option to point to the Erlang root directory:

  ```bash
  .../configure --with-erlang=/erlang/root/directory
  ```

* using the `$ERL` variable to point to the alternate `erl(1)` binary:

  ```bash
  .../configure ERL=/path/to/erl
  ```

### Compiling

Easy peasy Japanesey!
```bash
make
```

You can use multiple make jobs (ie. using the `-j` option). However Erlang modules are built using Erlang's `make` application. And, as of this writing (Erlang R15B03), this application doesn't build modules in parallel.

## Installing

* Simply run:

  ```bash
  make install
  ```

  Note that you may need increased privileges.

* To help mostly packagers, the `$DESTDIR` variable is honored:

  ```bash
  make install DESTDIR=/path/to/fake/root
  ```
