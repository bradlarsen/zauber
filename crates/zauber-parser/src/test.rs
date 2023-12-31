use super::*;

use indoc::indoc;

#[test]
fn lua_parses() {
    let magic_input = indoc!(r"
        #------------------------------------------------------------------------------
        # $File: lua,v 1.8 2020/10/08 23:23:56 christos Exp $
        # lua:  file(1) magic for Lua scripting language
        # URL:  https://www.lua.org/
        # From: Reuben Thomas <rrt@sc3d.org>, Seo Sanghyeon <tinuviel@sparcs.kaist.ac.kr>

        # Lua scripts
        0	search/1/w	#!\ /usr/bin/lua	Lua script text executable
        !:mime	text/x-lua
        0	search/1/w	#!\ /usr/local/bin/lua	Lua script text executable
        !:mime	text/x-lua
        0	search/1	#!/usr/bin/env\ lua	Lua script text executable
        !:mime	text/x-lua
        0	search/1	#!\ /usr/bin/env\ lua	Lua script text executable
        !:mime	text/x-lua

        # Lua bytecode
        0	string		\033Lua			Lua bytecode,
        # 2.4 uses 0x23 as its version byte because it shares the format
        # with 2.3 (which was never released publicly).
        >4	byte		0x23			version 2.4
        >4	byte		0x25			version 2.5/3.0
        >4	byte		0x31			version 3.1
        >4	byte		0x32			version 3.2
        >4	byte		0x40			version 4.0
        >4	byte		0x50			version 5.0
        >4	byte		0x51			version 5.1
        >4	byte		0x52			version 5.2
        >4	byte		0x53			version 5.3
        >4	byte		0x54			version 5.4
    ");

    parse_magic_file_contents(magic_input, None).unwrap();
}
