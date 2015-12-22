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
import static net.ohloh.ohcount4j.Language.BAT;

import org.testng.annotations.Test;

/**
 * @author mpujari
 *
 */
public class BatScannerTest extends BaseScannerTest {

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
