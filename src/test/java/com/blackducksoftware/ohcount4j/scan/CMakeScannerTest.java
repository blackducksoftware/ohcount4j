/**
 * Copyright 2016 Black Duck Software, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.blackducksoftware.ohcount4j.scan;

import static com.blackducksoftware.ohcount4j.Entity.BLANK;
import static com.blackducksoftware.ohcount4j.Entity.CODE;
import static com.blackducksoftware.ohcount4j.Entity.COMMENT;
import static com.blackducksoftware.ohcount4j.Language.CMake;

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
