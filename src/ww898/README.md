# UTF-8/16/32 C++ library
This is the C++11 template based header only library under Windows/Linux/MacOs to convert UFT-8/16/32 symbols and strings. The library transparently support `wchar_t` as UTF-16 for Windows and UTF-32 for Linux and MacOs.

UTF-8 and UTF-32 (UCS-32) both support 31 bit wide code points `[0‥0x7FFFFFFF]`with no restriction. UTF-16 supports only unicode code points `[0‥0x10FFFF]`, where high `[0xD800‥0xDBFF]` and low `[0xDC00‥0xDFFF]` surrogate regions are prohibited.

The maximum UTF-16 symbol size is 2 words (4 bytes, both words should be in the surrogate region). UFT-32 (UCS-32) is always 1 word (4 bytes). UTF-8 has the maximum symbol size (see [conversion table](#utf-8-conversion-table) for details):
- 4 bytes for unicode code points
- 6 bytes for 31bit code points

###### UTF-16 surrogate decoder:
|High\Low|DC00|DC01|…|DFFF|
|:-:|:-:|:-:|:-:|:-:|
|**D800**|010000|010001|…|0103FF|
|**D801**|010400|010401|…|0107FF|
|**⋮**|⋮|⋮|⋱|⋮|
|**DBFF**|10FC00|10FC01|…|10FFFF|

![UTF-16 Surrogates](https://upload.wikimedia.org/wikipedia/commons/thumb/b/b8/Utf-16.svg/512px-Utf-16.svg.png)

## Supported compilers

Tested on following compilers:
- [Visual Studio 2013 v12.0.40629.00 Update 5](perf/vc120_win.md)
- [Visual Studio 2015 v14.0.25431.01 Update 3](perf/vc140_win.md)
- [Visual Studio 2017 v15.6.7](perf/vc141_win.md)
- [Visual Studio 2019 v16.0.3](perf/vc142_win.md)
- [GNU v5.4.0](perf/gnu_linux.md)
- [Clang v6.0.1](perf/clang_linux.md)
- [Apple Clang v10.0.1](perf/clang_mac.md)

## Usage example

```cpp
    // यूनिकोड
    static char const u8s[] = "\xE0\xA4\xAF\xE0\xA5\x82\xE0\xA4\xA8\xE0\xA4\xBF\xE0\xA4\x95\xE0\xA5\x8B\xE0\xA4\xA1";
    using namespace ww898::utf;
    std::u16string u16;
    convz<utf_selector_t<decltype(*u8s)>, utf16>(u8s, std::back_inserter(u16));
    std::u32string u32;
    conv<utf16, utf_selector_t<decltype(u32)::value_type>>(u16.begin(), u16.end(), std::back_inserter(u32));
    std::vector<char> u8;
    convz<utf32, utf8>(u32.data(), std::back_inserter(u8));
    std::wstring uw;
    conv<utf8, utfw>(u8s, u8s + sizeof(u8s), std::back_inserter(uw));
    auto u8r = conv<char>(uw);
    auto u16r = conv<char16_t>(u16);
    auto uwr = convz<wchar_t>(u8s);

    auto u32r = conv<char32_t>(std::string_view(u8r.data(), u8r.size())); // C++17 only

    static_assert(std::is_same<utf_selector<decltype(*u8s)>, utf_selector<decltype(u8)::value_type>>::value, "Fail");
    static_assert(
        std::is_same<utf_selector_t<decltype(u16)::value_type>, utf_selector_t<decltype(uw)::value_type>>::value !=
        std::is_same<utf_selector_t<decltype(u32)::value_type>, utf_selector_t<decltype(uw)::value_type>>::value, "Fail");
```

## UTF-8 Conversion table
![UTF-8/32 table](https://upload.wikimedia.org/wikipedia/commons/3/38/UTF-8_Encoding_Scheme.png)
