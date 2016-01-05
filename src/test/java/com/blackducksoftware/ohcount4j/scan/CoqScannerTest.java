/*
 * Copyright (C) 2016 Black Duck Software Inc.
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
import static com.blackducksoftware.ohcount4j.Language.COQ;

import org.testng.annotations.Test;

/**
 * @author mpujari
 *
 */
public class CoqScannerTest extends AbstractBaseScannerTest {

    @Test
    public void basic() {
        assertLine(COQ, new Line(COQ, BLANK), "\n");
        assertLine(COQ, new Line(COQ, BLANK), "     \n");
        assertLine(COQ, new Line(COQ, BLANK), "\t\n");
        assertLine(COQ, new Line(COQ, CODE), "pragma(msg, Format!(\"7! = %s\", fact_7));\n");
        assertLine(COQ, new Line(COQ, COMMENT), "(* Line comment\n");
        assertLine(COQ, new Line(COQ, COMMENT), "(* Block comment test 1 *)\n");
        assertLine(COQ, new Line(COQ, COMMENT), "(* Block comment *)\n");
        assertLine(COQ, new Line(COQ, COMMENT), "(*\n");
        assertLine(COQ, new Line(COQ, CODE), "mixin(fooToD(import(\"example.foo\"))); (* with comment\n");
    }

    @Test
    public void eofHandling() {
        // Note lack of trailing \n in all cases below
        assertLine(COQ, new Line(COQ, BLANK), "     ");
        assertLine(COQ, new Line(COQ, BLANK), "\t");
        assertLine(COQ, new Line(COQ, CODE), "let rec typeCheck = function");
        assertLine(COQ, new Line(COQ, COMMENT), "(* Line comment");
        assertLine(COQ, new Line(COQ, COMMENT), "(* Block comment *)");
        assertLine(COQ, new Line(COQ, COMMENT), "(* Block comment *)");
        assertLine(COQ, new Line(COQ, COMMENT), "(*");
        assertLine(COQ, new Line(COQ, CODE), "| Nat n -> Found TNat (* with comment");
        assertLine(COQ, new Line(COQ, CODE), "(match typeCheck e1 with (* with comment");
    }

    @Test
    public void sampleTest() {
        String code = "(* Simple Coq Program\n"
                + "         (* For Testing Purposes\n"
                + "\t\n"
                + "         *)\n"
                + "*)\n"
                + "Definition hd n (ls : ilist (S n)) : A :=\n"
                + "match ls with\n"
                + "| Nil => whatGoesHere?\n"
                + "| Cons h => h\n"
                + "end.\n"
                + "\n";

        Line[] expected = {
                new Line(COQ, COMMENT),
                new Line(COQ, COMMENT),
                new Line(COQ, BLANK),
                new Line(COQ, COMMENT),
                new Line(COQ, COMMENT),
                new Line(COQ, CODE),
                new Line(COQ, CODE),
                new Line(COQ, CODE),
                new Line(COQ, CODE),
                new Line(COQ, CODE),
                new Line(COQ, BLANK),
        };
        assertLines(COQ, expected, code);
    }

    @Test
    public void unterminatedNestedBlockCommentCrash() {
        // This minimal case caused an Arrays.copyOfRange() crash
        String code = "(*\n (*\n +/\n\n\n";

        Line[] expected = {
                new Line(COQ, COMMENT),
                new Line(COQ, COMMENT),
                new Line(COQ, COMMENT),
                new Line(COQ, BLANK),
                new Line(COQ, BLANK)
        };
        assertLines(COQ, expected, code);
    }

}
