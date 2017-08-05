
0.3.2
=====

Bug fixes:
 * Fixed a bug that prevented wrapping sometimes.

0.3.1
=====

Bug fixes:
 * Fix inconsistent long token breaking (long tokens anywhere but the
   beginning of a line)

0.3
===

API changes:
 * Added the breakLongWords setting to WrapSettings. This setting makes
   it possible to cause words to get broken up over multiple lines if
   their lengths exceed the wrapping width.

0.2
===

API changes:
 * Added a WrapSettings data type for controlling wrapping behavior.
 * All functions now require a WrapSettings.
 * Added defaultWrapSettings for prior behavior.
 * Wrap settings now include a setting to control how indentation is
   preserved in broken lines.

Bug fixes:
 * Lines with only whitespace are preserved as empty lines.

0.1.2
=====

Bug fixes:
 * Fixed a bug where multiple consecutive newlines were not properly preserved
   as advertised (#2)

0.1.1
=====

Package changes:
 * Removed a duplicate mention of the changelog file in the cabal
   package description that used the wrong filename case (#1)

0.1
===

* First version.
