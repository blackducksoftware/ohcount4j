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
import static net.ohloh.ohcount4j.Language.BATCH;

import org.testng.annotations.Test;

/**
 * @author mpujari
 *
 */
public class BatchScannerTest extends BaseScannerTest {

    @Test
    public void basic() {
        assertLine(BATCH, new Line(BATCH, BLANK), "\n");
        assertLine(BATCH, new Line(BATCH, BLANK), "     \n");
        assertLine(BATCH, new Line(BATCH, BLANK), "\t\n");
        assertLine(BATCH, new Line(BATCH, CODE), "echo \"hello\"\n");
        assertLine(BATCH, new Line(BATCH, COMMENT), "ReM Line comment\n");
        assertLine(BATCH, new Line(BATCH, COMMENT), "@rEm\n");
        assertLine(BATCH, new Line(BATCH, CODE), "ls rem with comment\n");
    }

    @Test
    public void eofHandling() {
        // Note lack of trailing \n in all cases below
        assertLine(BATCH, new Line(BATCH, BLANK), "     ");
        assertLine(BATCH, new Line(BATCH, BLANK), "\t");
        assertLine(BATCH, new Line(BATCH, CODE), "echo \"hello\"");
        assertLine(BATCH, new Line(BATCH, COMMENT), "REM Line comment");
        assertLine(BATCH, new Line(BATCH, COMMENT), "rEM Line comment");
        assertLine(BATCH, new Line(BATCH, COMMENT), "@REM Line comment");
        assertLine(BATCH, new Line(BATCH, COMMENT), ":: Line comment");
        assertLine(BATCH, new Line(BATCH, CODE), ":goto_run1");
        assertLine(BATCH, new Line(BATCH, COMMENT), "REm");
        assertLine(BATCH, new Line(BATCH, CODE), "ls REM with comment");
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
                new Line(BATCH, CODE),
                new Line(BATCH, CODE),
                new Line(BATCH, CODE),
                new Line(BATCH, CODE),
                new Line(BATCH, BLANK),
                new Line(BATCH, CODE),
                new Line(BATCH, CODE),
                new Line(BATCH, COMMENT),
                new Line(BATCH, CODE),
                new Line(BATCH, BLANK),
                new Line(BATCH, COMMENT),
        };
        assertLines(BATCH, expected, code);
    }

}
