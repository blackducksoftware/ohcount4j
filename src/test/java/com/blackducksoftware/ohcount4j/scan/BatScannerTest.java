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
import static com.blackducksoftware.ohcount4j.Language.BAT;

import org.testng.annotations.Test;

/**
 * @author mpujari
 *
 */
public class BatScannerTest extends AbstractBaseScannerTest {

    @Test
    public void basic() {
        assertLine(BAT, new Line(BAT, BLANK), "\n");
        assertLine(BAT, new Line(BAT, BLANK), "     \n");
        assertLine(BAT, new Line(BAT, BLANK), "\t\n");
        assertLine(BAT, new Line(BAT, CODE), "echo \"hello\"\n");
        assertLine(BAT, new Line(BAT, COMMENT), "ReM Line comment\n");
        assertLine(BAT, new Line(BAT, COMMENT), "@rEm\n");
        assertLine(BAT, new Line(BAT, CODE), "ls rem with comment\n");
    }

    @Test
    public void eofHandling() {
        // Note lack of trailing \n in all cases below
        assertLine(BAT, new Line(BAT, BLANK), "     ");
        assertLine(BAT, new Line(BAT, BLANK), "\t");
        assertLine(BAT, new Line(BAT, CODE), "echo \"hello\"");
        assertLine(BAT, new Line(BAT, COMMENT), "REM Line comment");
        assertLine(BAT, new Line(BAT, COMMENT), "rEM Line comment");
        assertLine(BAT, new Line(BAT, COMMENT), "@REM Line comment");
        assertLine(BAT, new Line(BAT, COMMENT), ":: Line comment");
        assertLine(BAT, new Line(BAT, CODE), ":goto_run1");
        assertLine(BAT, new Line(BAT, COMMENT), "REm");
        assertLine(BAT, new Line(BAT, CODE), "ls REM with comment");
    }

    @Test
    public void simpleTest() {
        String code = "@echo off\n"
                + "call :sayhello result=world\n"
                + "echo %result%\n"
                + "exit /b\n"
                + "\n"
                + ":sayhello\n"
                + "set %1=Hello %2\n"
                + "REM Set %1 to set the returning value\n"
                + "exit /b\n"
                + "\n"
                + ":: this is a comment";

        Line[] expected = {
                new Line(BAT, CODE),
                new Line(BAT, CODE),
                new Line(BAT, CODE),
                new Line(BAT, CODE),
                new Line(BAT, BLANK),
                new Line(BAT, CODE),
                new Line(BAT, CODE),
                new Line(BAT, COMMENT),
                new Line(BAT, CODE),
                new Line(BAT, BLANK),
                new Line(BAT, COMMENT),
        };
        assertLines(BAT, expected, code);
    }

}
