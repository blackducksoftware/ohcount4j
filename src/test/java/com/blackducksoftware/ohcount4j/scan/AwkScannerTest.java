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
package com.blackducksoftware.ohcount4j.scan;

import static com.blackducksoftware.ohcount4j.Entity.BLANK;
import static com.blackducksoftware.ohcount4j.Entity.CODE;
import static com.blackducksoftware.ohcount4j.Entity.COMMENT;
import static com.blackducksoftware.ohcount4j.Language.AWK;

import org.testng.annotations.Test;

/**
 * @author mpujari
 *
 */
public class AwkScannerTest extends AbstractBaseScannerTest {

    @Test
    public void basic() {
        assertLine(AWK, new Line(AWK, BLANK), "\n");
        assertLine(AWK, new Line(AWK, BLANK), "     \n");
        assertLine(AWK, new Line(AWK, BLANK), "\t\n");
        assertLine(AWK, new Line(AWK, CODE), "echo \"hello\"\n");
        assertLine(AWK, new Line(AWK, CODE), "BEGIN { ");
        assertLine(AWK, new Line(AWK, COMMENT), "# Line comment\n");
        assertLine(AWK, new Line(AWK, COMMENT), "#\n");
        assertLine(AWK, new Line(AWK, COMMENT), "#   ");
        assertLine(AWK, new Line(AWK, CODE), "ls # this is not a comment\n");
    }

    @Test
    public void eofHandling() {
        assertLine(AWK, new Line(AWK, BLANK), "     ");
        assertLine(AWK, new Line(AWK, BLANK), "\t");
        assertLine(AWK, new Line(AWK, CODE), "echo \"hello\"");
        assertLine(AWK, new Line(AWK, COMMENT), "# Line comment");
        assertLine(AWK, new Line(AWK, COMMENT), "#");
        assertLine(AWK, new Line(AWK, CODE), "ls # with comment");
    }

    @Test
    public void simpleTest() {
        String code = "BEGIN {\n"
                + "# Print the squares from 1 to 10 the first way\n"
                + "i=1;\n"
                + " while (i <= 10) {\n"
                + "  printf \"The square of \", i, \" is \", i*i;\n"
                + "  i = i+1;\n"
                + " }\n"
                + "\n"
                + "# do it again, using more concise code\n"
                + " for (i=1; i <= 10; i++) {\n"
                + "  printf \"The square of \", i, \" is \", i*i;\n"
                + " }\n"
                + "# now end\n"
                + "exit;\n"
                + "}\n";
        ;

        Line[] expected = {
                new Line(AWK, CODE),
                new Line(AWK, COMMENT),
                new Line(AWK, CODE),
                new Line(AWK, CODE),
                new Line(AWK, CODE),
                new Line(AWK, CODE),
                new Line(AWK, CODE),
                new Line(AWK, BLANK),
                new Line(AWK, COMMENT),
                new Line(AWK, CODE),
                new Line(AWK, CODE),
                new Line(AWK, CODE),
                new Line(AWK, COMMENT),
                new Line(AWK, CODE),
                new Line(AWK, CODE),
        };
        assertLines(AWK, expected, code);
    }

}
