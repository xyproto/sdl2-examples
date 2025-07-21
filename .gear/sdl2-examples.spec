%define _unpackaged_files_terminate_build 1

Name: sdl2-examples
Version: 1.0
Release: alt1

Summary: "hello world" for SDL2 for various programming languages
License: BSD-3-Clause
Group: Development/Other
Url: https://github.com/xyproto/sdl2-examples.git
Vcs: https://github.com/xyproto/sdl2-examples.git

Source0: %name-%version.tar 

%description
"hello world" for SDL2 for various programming languages.
Each sample creates a window, displays an image, then waits 
two seconds and quits. 

All executables should ideally build and run on Linux, macOS,
Windows, BSD* and more, but they should at least work on Linux. 

Most subdirectories contains README.md files with more details,
and a Makefile to have one way of building each sample.

For newer versions of macOS, the programs also appear to need 
an event loop for the window to show up, so I'm in the process 
of adding that to each example. The window just isn't shown if
there is no event loop.

%prep
%setup

%build

%install

%files
%doc README.md LICENSE COMPILES.md

%changelog
* Mon Jul 21 2025 Fedor Moseichuck <fedor@altlinux.org> 1.0-alt1
- Initial build for Sisyphus.
