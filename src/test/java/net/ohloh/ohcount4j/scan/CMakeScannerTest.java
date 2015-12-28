/*
 * Copyright (C) 2015 Black Duck Software Inc.
 * http://www.blackducksoftware.com/
 * All rights reserved.
 * 
 * This software is the confidential and proprietary information of
 * Black Duck Software ("Confidential Information"). You shall not
 * disclose such Confidential Information and shall use it only in
 * accordance with the terms of the license agreement you entered into
 * with Black Duck Software.
 */
package net.ohloh.ohcount4j.scan;

import static net.ohloh.ohcount4j.Entity.BLANK;
import static net.ohloh.ohcount4j.Entity.CODE;
import static net.ohloh.ohcount4j.Entity.COMMENT;
import static net.ohloh.ohcount4j.Language.CMake;

import org.testng.annotations.Test;

/**
 * @author mpujari
 *
 */
public class CMakeScannerTest extends AbstractBaseScannerTest {

    @Test
    public void basic() {
        assertLine(CMake, new Line(CMake, BLANK), "\n");
        assertLine(CMake, new Line(CMake, BLANK), "     \n");
        assertLine(CMake, new Line(CMake, BLANK), "\t\n");
        assertLine(CMake, new Line(CMake, CODE), "\"${CMAKE_CURRENT_SOURCE_DIR}/DartLocal.conf.in\"\n");
        assertLine(CMake, new Line(CMake, COMMENT), "# Line comment\n");
        assertLine(CMake, new Line(CMake, COMMENT), "#\n");
        assertLine(CMake, new Line(CMake, COMMENT), "\t#\n");
        assertLine(CMake, new Line(CMake, CODE), "SET(CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)\n");
        assertLine(CMake, new Line(CMake, CODE), "if (${WIN32}) # with comment\n");
    }

    @Test
    public void eofHandling() {
        // Note lack of trailing \n in all cases below
        assertLine(CMake, new Line(CMake, BLANK), "     ");
        assertLine(CMake, new Line(CMake, BLANK), "\t");
        assertLine(CMake, new Line(CMake, CODE), "include(Source/CMakeVersionCompute.cmake)");
        assertLine(CMake, new Line(CMake, COMMENT), "# Line comment");
        assertLine(CMake, new Line(CMake, COMMENT), "#");
        assertLine(CMake, new Line(CMake, CODE), "include (${CMAKE_ROOT}/Modules/Dart.cmake) # with comment");
    }

    @Test
    public void simpleTest() {
        String code = "if (${UNIX})\n"
                + "  set (DESKTOP $ENV{HOME})\n"
                + "else()\n"
                + "  set (DESKTOP $ENV{USERPROFILE}/Desktop)\n"
                + "endif()\n"
                + "\n"
                + "# comment 1\n"
                + "\t# comment 1\n"
                + "set  (PRJ      ${DESKTOP}/common/svn )\n"
                + "set  (FILELIST ${PRJ}/src/source.txt )\n";

        Line[] expected = {
                new Line(CMake, CODE),
                new Line(CMake, CODE),
                new Line(CMake, CODE),
                new Line(CMake, CODE),
                new Line(CMake, CODE),
                new Line(CMake, BLANK),
                new Line(CMake, COMMENT),
                new Line(CMake, COMMENT),
                new Line(CMake, CODE),
                new Line(CMake, CODE),
        };
        assertLines(CMake, expected, code);
    }

}
