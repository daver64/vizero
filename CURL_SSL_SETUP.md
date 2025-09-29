# Instructions to Install CURL with SSL for MSVC

## Option 1: vcpkg (Recommended)

1. **Install vcpkg** (if you don't have it):
```cmd
git clone https://github.com/Microsoft/vcpkg.git C:\vcpkg
cd C:\vcpkg
.\bootstrap-vcpkg.bat
```

2. **Install CURL with SSL**:
```cmd
.\vcpkg install curl[ssl]:x64-windows
```

3. **Update Environment Variable**:
```cmd
set CURL_ROOT=C:\vcpkg\installed\x64-windows
```

4. **Rebuild Vizero**:
```cmd
cd C:\Users\daver\source\vizero
build.bat
```

## Option 2: Download Pre-built MSVC CURL

1. Visit: https://curl.se/windows/
2. Download a build marked "Visual Studio" or "MSVC"
3. Extract to C:\Users\daver\LocalApps\curl
4. Make sure it has:
   - lib/libcurl.lib (or libcurl_imp.lib)  
   - bin/libcurl.dll
   - include/curl/curl.h

## What You Need

The CURL build must have:
- ✅ **SSL/TLS support** (OpenSSL, Schannel, or similar)
- ✅ **MSVC-compatible libraries** (.lib files, not .a files)
- ✅ **HTTPS protocol** support

Your current MinGW CURL has SSL support but incompatible library format for MSVC.

## Testing

After installing MSVC-compatible CURL:
1. Build Vizero: `build.bat`
2. Run Vizero: `vizero.exe`  
3. Test Claude: `:llm-test`

You should see successful HTTPS connection to Claude API.