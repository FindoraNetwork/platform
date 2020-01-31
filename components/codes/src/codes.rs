// Exit codes and descriptions are copied from:
// https://www.freebsd.org/cgi/man.cgi?query=sysexits&apropos=0&sektion=0&manpath=FreeBSD+4.3-RELEASE&format=html
pub mod exit_code {
  // The command was used incorrectly, e.g., with the
  // wrong number of arguments, a bad flag, a bad syntax
  // in a parameter, or whatever.
  pub const EX_USAGE: i32 = 64;

  // The input data was incorrect in some way.  This
  // should only be used for user's data and not system
  // files.
  pub const EX_DATAERR: i32 = 65;

  // An input file (not a system file) did not exist or
  // was not readable.  This could also include errors
  // like ``No message'' to a mailer (if it cared to
  // catch it).
  pub const EX_NOINPUT: i32 = 66;

  // The user specified did not exist.  This might be
  // used for mail addresses or remote logins.
  pub const EX_NOUSER: i32 = 67;

  // The host specified did not exist.  This is used in
  // mail addresses or network requests.
  pub const EX_NOHOST: i32 = 68;

  // A service is unavailable.  This can occur if a sup­
  // port program or file does not exist.  This can also
  // be used as a catchall message when something you
  // wanted to do doesn't work, but you don't know why.
  pub const EX_UNAVAILABLE: i32 = 69;

  // An internal software error has been detected.  This
  // should be limited to non-operating system related
  // errors as possible.
  pub const EX_SOFTWARE: i32 = 70;

  // An operating system error has been detected.  This
  // is intended to be used for such things as ``cannot
  // fork'', ``cannot create pipe'', or the like.  It
  // includes things like getuid returning a user that
  // does not exist in the passwd file.
  pub const EX_OSERR: i32 = 71;

  // Some system file (e.g., /etc/passwd, /var/run/utmp,
  // etc.) does not exist, cannot be opened, or has some
  // sort of error (e.g., syntax error).
  pub const EX_OSFILE: i32 = 72;

  // A (user specified) output file cannot be created.
  pub const EX_CANTCREATE: i32 = 73;

  // An error occurred while doing I/O on some file.
  pub const EX_IOERR: i32 = 74;

  // Temporary failure, indicating something that is not
  // really an error.  In sendmail, this means that a
  // mailer (e.g.) could not create a connection, and
  // the request should be reattempted later.
  pub const EX_TEMPFAIL: i32 = 75;

  // The remote system returned something that was ``not
  // possible'' during a protocol exchange.
  pub const EX_PROTOCOL: i32 = 76;

  // You did not have sufficient permission to perform
  // the operation.  This is not intended for file sys­
  // tem problems, which should use EX_NOINPUT or
  // EX_CANTCREAT, but rather for higher level permis­
  // sions.
  pub const EX_NOPERM: i32 = 77;

  // Something was found in an unconfigured or miscon­
  // figured state.
  pub const EX_CONFIG: i32 = 78;
}
