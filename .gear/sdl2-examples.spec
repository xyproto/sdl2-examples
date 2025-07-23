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
BuildRequires: libSDL2_image-devel
BuildRequires: libSDL2-devel
BuildRequires: gcc-c++
BuildRequires: cmake

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
 - C++23 CMake
 - C++20 CMake
 - C++17 CMake
 - C++14 CMake
 - C++11 CMake
 - C++11

%prep
%setup

# https://packages.altlinux.org/en/sisyphus/srpms/python-module-sdl2/
# This module is removed, so i decided to change version of python
sed -i '1s|/usr/bin/env python|/usr/bin/python3|' python/main.py

sed -i 's|IMGDIR="../img/"|IMGDIR="%_datadir/%name/img/"|' c++23-cmake/CMakeLists.txt
sed -i 's|IMGDIR="../img/"|IMGDIR="%_datadir/%name/img/"|' c++20-cmake/CMakeLists.txt
sed -i 's|IMGDIR="../img/"|IMGDIR="%_datadir/%name/img/"|' c++17-cmake/CMakeLists.txt
sed -i 's|IMGDIR="../img/"|IMGDIR="%_datadir/%name/img/"|' c++14-cmake/CMakeLists.txt
sed -i 's|IMGDIR="../img/"|IMGDIR="%_datadir/%name/img/"|' c++11-cmake/CMakeLists.txt

sed -i 's|"../img/grumpy-cat.bmp"|"%_datadir/%name/img/grumpy-cat.bmp"|' c++11/main.cpp

%build

python3 -m compileall python/

pushd c++23-cmake
%cmake
%cmake_build
popd

pushd c++20-cmake
%cmake
%cmake_build
popd

pushd c++17-cmake
%cmake
%cmake_build
popd

pushd c++14-cmake
%cmake
%cmake_build
popd

pushd c++11-cmake
%cmake
%cmake_build
popd

pushd c++11
%make_build
popd

%install
mkdir -p \
  %buildroot%_datadir/%name/python \
  %buildroot%_datadir/%name/cpp11 \
  %buildroot%_bindir \
  %buildroot%_desktopdir \
  %buildroot%_iconsdir/hicolor/64x64/apps \
  %buildroot%_datadir/%name/img \

install -pm 644 img/grumpy-cat.bmp %buildroot%_datadir/%name/img/

# Python
cp -a python/* %buildroot%_datadir/%name/python

cat <<'EOF' > %buildroot%_bindir/sdl2-python-example
#!/bin/sh
cd %_datadir/%name/python
exec ./main.py
EOF

chmod +x %buildroot%_bindir/sdl2-python-example

# C++23 cmake
install -p -m 755 c++17-cmake/x86_64-alt-linux/main %buildroot%_bindir/sdl2-cpp23-cmake-example

# C++20 cmake
install -p -m 755 c++20-cmake/x86_64-alt-linux/main %buildroot%_bindir/sdl2-cpp20-cmake-example

# C++17 cmake
install -p -m 755 c++17-cmake/x86_64-alt-linux/main %buildroot%_bindir/sdl2-cpp17-cmake-example

# C++14 cmake
install -p -m 755 c++17-cmake/x86_64-alt-linux/main %buildroot%_bindir/sdl2-cpp14-cmake-example

# C++11 cmake
install -p -m 755 c++17-cmake/x86_64-alt-linux/main %buildroot%_bindir/sdl2-cpp11-cmake-example

# C++11
install -p -m 755 c++11/main %buildroot%_bindir/sdl2-cpp11-example

cat << EOF > %buildroot%_desktopdir/sdl2-python-example.desktop
[Desktop Entry]
Version=1.0
Type=Application
Name=SDL2 Python Example
Exec=sdl2-python-example
Icon=grumpy-cat
Categories=Other;
EOF

cat << EOF > %buildroot%_desktopdir/sdl2-cpp23-cmake-example.desktop
[Desktop Entry]
Version=1.0
Type=Application
Name=SDL2 C++23 CMake Example
Exec=sdl2-cpp23-cmake-example
Icon=grumpy-cat
Categories=Other;
EOF

cat << EOF > %buildroot%_desktopdir/sdl2-cpp20-cmake-example.desktop
[Desktop Entry]
Version=1.0
Type=Application
Name=SDL2 C++20 CMake Example
Exec=sdl2-cpp20-cmake-example
Icon=grumpy-cat
Categories=Other;
EOF

cat << EOF > %buildroot%_desktopdir/sdl2-cpp17-cmake-example.desktop
[Desktop Entry]
Version=1.0
Type=Application
Name=SDL2 C++17 CMake Example
Exec=sdl2-cpp17-cmake-example
Icon=grumpy-cat
Categories=Other;
EOF

cat << EOF > %buildroot%_desktopdir/sdl2-cpp14-cmake-example.desktop
[Desktop Entry]
Version=1.0
Type=Application
Name=SDL2 C++14 CMake Example
Exec=sdl2-cpp14-cmake-example
Icon=grumpy-cat
Categories=Other;
EOF

cat << EOF > %buildroot%_desktopdir/sdl2-cpp11-cmake-example.desktop
[Desktop Entry]
Version=1.0
Type=Application
Name=SDL2 C++11 CMake Example
Exec=sdl2-cpp11-cmake-example
Icon=grumpy-cat
Categories=Other;
EOF

cat << EOF > %buildroot%_desktopdir/sdl2-cpp11-example.desktop
[Desktop Entry]
Version=1.0
Type=Application
Name=SDL2 C++11 Example
Exec=sdl2-cpp11-example
Icon=grumpy-cat
Categories=Other;
EOF

install -pm 644 img/grumpy-cat.png %buildroot%_iconsdir/hicolor/64x64/apps/grumpy-cat.png

%files
%doc README.md LICENSE COMPILES.md

%_datadir/%name/img/
%_desktopdir/*.desktop
%_iconsdir/hicolor/64x64/apps/grumpy-cat.png

%_bindir/sdl2-python-example
%_datadir/%name/python/

%_bindir/sdl2-cpp23-cmake-example

%_bindir/sdl2-cpp20-cmake-example

%_bindir/sdl2-cpp17-cmake-example

%_bindir/sdl2-cpp14-cmake-example

%_bindir/sdl2-cpp11-cmake-example

%_bindir/sdl2-cpp11-example

%changelog
* Mon Jul 21 2025 Fedor Moseichuck <fedor@altlinux.org> 1.0-alt1
- Initial build for Sisyphus.
