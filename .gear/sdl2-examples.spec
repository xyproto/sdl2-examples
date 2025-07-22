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

BuildRequires: python3-dev
BuildRequires: python3-module-sdl2

%description
"hello world" for SDL2 for various programming languages.
Each sample creates a window, displays a grumpy cat image,
then waits two seconds and quits.

All executables should ideally build and run on Linux, macOS,
Windows, BSD* and more, but they should at least work on Linux. 

Most subdirectories contains README.md files with more details,
and a Makefile to have one way of building each sample.

For newer versions of macOS, the programs also appear to need 
an event loop for the window to show up, so I'm in the process 
of adding that to each example. The window just isn't shown if
there is no event loop.

Currently packaged examples:
 - Python

%prep
%setup

# https://packages.altlinux.org/en/sisyphus/srpms/python-module-sdl2/ this module is removed,
# so i decided to change version of python
sed -i '1s|/usr/bin/env python|/usr/bin/python3|' python/main.py

%build

python3 -m compileall python/

%install
mkdir -p \
  %buildroot%_datadir/%name/python \
  %buildroot%_bindir \
  %buildroot%_desktopdir \
  %buildroot%_iconsdir/hicolor/48x48/apps \

cp -a python/* %buildroot%_datadir/%name/python
mkdir %buildroot%_datadir/%name/img
install -pm 644 img/grumpy-cat.bmp %buildroot%_datadir/%name/img/

cat <<'EOF' > %buildroot%_bindir/sdl2-python-example
#!/bin/sh
cd %_datadir/%name/python
exec python3 main.py
EOF

chmod +x %buildroot%_bindir/sdl2-python-example

cat << EOF > %buildroot%_desktopdir/sdl2-python-example.desktop
[Desktop Entry]
Version=1.0
Type=Application
Name=SDL2 Python Example
Exec=sdl2-python-example
Icon=grumpy-cat
Categories=Other;
EOF

install -pm 644 img/grumpy-cat.png %buildroot%_iconsdir/hicolor/48x48/apps/grumpy-cat.png

%files
%doc README.md LICENSE COMPILES.md
%_bindir/sdl2-python-example
%_datadir/%name/python
%_desktopdir/sdl2-python-example.desktop
%_iconsdir/hicolor/48x48/apps/grumpy-cat.png
%_datadir/%name/img

%changelog
* Mon Jul 21 2025 Fedor Moseichuck <fedor@altlinux.org> 1.0-alt1
- Initial build for Sisyphus.
