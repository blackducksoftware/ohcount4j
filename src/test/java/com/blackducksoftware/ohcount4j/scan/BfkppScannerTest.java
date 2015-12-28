/**
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

import org.testng.annotations.Test;

import com.blackducksoftware.ohcount4j.Language;

/**
 * @author gandhip
 *
 */
public class BfkppScannerTest extends AbstractBaseScannerTest {

    @Test
    public void basic() {
        assertLine(Language.BFPP, new Line(Language.BFPP, BLANK), "\n");
        assertLine(Language.BFPP, new Line(Language.BFPP, BLANK), "     \n");
        assertLine(Language.BFPP, new Line(Language.BFPP, BLANK), "\t\n");
        assertLine(Language.BFPP, new Line(Language.BFPP, CODE), ">++++++[>+++[<<++++>>-]<-]<.\n");
        assertLine(Language.BFPP, new Line(Language.BFPP, CODE), "+++++++.\n");
        assertLine(Language.BFPP, new Line(Language.BFPP, COMMENT), "= Line that does nothing:\n");
        assertLine(Language.BFPP, new Line(Language.BFPP, CODE), "@include(nothing.bfpp)\n");
        assertLine(Language.BFPP, new Line(Language.BFPP, CODE), "#[\n");
    }

    @Test
    public void eofHandling() {
        // Note lack of trailing \n in all cases below
        assertLine(Language.BFPP, new Line(Language.BFPP, BLANK), "     ");
        assertLine(Language.BFPP, new Line(Language.BFPP, BLANK), "\t");
        assertLine(Language.BFPP, new Line(Language.BFPP, CODE), ">++++++[>+++[<<++++>>-]<-]<.");
        assertLine(Language.BFPP, new Line(Language.BFPP, CODE), "+++++++.");
        assertLine(Language.BFPP, new Line(Language.BFPP, COMMENT), "= Line that does nothing:");
        assertLine(Language.BFPP, new Line(Language.BFPP, CODE), "@include(nothing.bfpp)");
        assertLine(Language.BFPP, new Line(Language.BFPP, CODE), "#[");
    }

    @Test
    public void testHelloWorld() throws Exception {
        String code = "\n"
                + "= comment\n"
                + ">++++++[>+++[<<++++>>-]<-]<.\n"
                + ">+++++[<++++++>-]<-.\n"
                + "+++++++.\n"
                + ".\n"
                + "+++.\n"
                + ">>++++[<++++++>-]<++[<--->-]<-.\n"
                + ">>++[<+++++>-]<+[<+++++>-]<.\n"
                + ">++++[<++++++>-]<.\n"
                + "+++.\n"
                + "------.\n"
                + ">++[<---->-]<.\n"
                + ">>++++[<++++>-]<+[<---->-]<.\n"
                + ">+++++++[<++++++>-]<-.\n"
                + ">++++++[<++++++>-]<+.\n"
                + ">>++++[<++++++>-]<++[<--->-]<.\n"
                + ">+++++[<+++++++>-]<-.\n"
                + ">++++[>++++[<<+++>>-]<-]<.\n"
                + ">++++[<---->-]<-.\n"
                + ">++[<++++>-]<.\n"
                + "+++++.\n"
                + ">++[<---->-]<.\n"
                + ">+++[<+++++>-]<.\n"
                + ">+++[<------>-]<.\n"
                + ">++[<++++>-]<.\n"
                + ">++++[>++++[<<---->>-]<-]<.\n"
                + ".\n"
                + ">++[<----->-]<.\n"
                + ".\n"
                + ".\n"
                + ".\n"
                + ".\n"
                + "[-]\n"
                + "  = [-] is used to clear a cell\n"
                + "\n"
                + "= Try to open b file\n"
                + ">+++++++[>+++++++[<<++>>-]<-]<.\n"
                + "#[\n"
                + "  >>++++[<++++>-]<+[<++++++>-]<.\n"
                + "  +++.\n"
                + "  +++.\n"
                + "  -------.\n"
                + "  >>++++[<++++++>-]<-[<--->-]<.\n"
                + "  >>++[<+++++>-]<+[<++++++>-]<.\n"
                + "  >>++[<+++++>-]<+[<------>-]<.\n"
                + "  >>++++[<++++++>-]<++[<+++>-]<+.\n"
                + "  +.\n"
                + "  >++[<----->-]<-.\n"
                + "  >+++[<+++>-]<.\n"
                + "  >+++[<--->-]<.\n"
                + "  -.\n"
                + "#]\n"
                + "@include(nothing.bfpp)\n";
        Line[] expected = {
                new Line(Language.BFPP, BLANK),
                new Line(Language.BFPP, COMMENT),
                new Line(Language.BFPP, CODE),
                new Line(Language.BFPP, CODE),
                new Line(Language.BFPP, CODE),
                new Line(Language.BFPP, CODE),
                new Line(Language.BFPP, CODE),
                new Line(Language.BFPP, CODE),
                new Line(Language.BFPP, CODE),
                new Line(Language.BFPP, CODE),
                new Line(Language.BFPP, CODE),
                new Line(Language.BFPP, CODE),
                new Line(Language.BFPP, CODE),
                new Line(Language.BFPP, CODE),
                new Line(Language.BFPP, CODE),
                new Line(Language.BFPP, CODE),
                new Line(Language.BFPP, CODE),
                new Line(Language.BFPP, CODE),
                new Line(Language.BFPP, CODE),
                new Line(Language.BFPP, CODE),
                new Line(Language.BFPP, CODE),
                new Line(Language.BFPP, CODE),
                new Line(Language.BFPP, CODE),
                new Line(Language.BFPP, CODE),
                new Line(Language.BFPP, CODE),
                new Line(Language.BFPP, CODE),
                new Line(Language.BFPP, CODE),
                new Line(Language.BFPP, CODE),
                new Line(Language.BFPP, CODE),
                new Line(Language.BFPP, CODE),
                new Line(Language.BFPP, CODE),
                new Line(Language.BFPP, CODE),
                new Line(Language.BFPP, CODE),
                new Line(Language.BFPP, CODE),
                new Line(Language.BFPP, COMMENT),
                new Line(Language.BFPP, BLANK),
                new Line(Language.BFPP, COMMENT),
                new Line(Language.BFPP, CODE),
                new Line(Language.BFPP, CODE),
                new Line(Language.BFPP, CODE),
                new Line(Language.BFPP, CODE),
                new Line(Language.BFPP, CODE),
                new Line(Language.BFPP, CODE),
                new Line(Language.BFPP, CODE),
                new Line(Language.BFPP, CODE),
                new Line(Language.BFPP, CODE),
                new Line(Language.BFPP, CODE),
                new Line(Language.BFPP, CODE),
                new Line(Language.BFPP, CODE),
                new Line(Language.BFPP, CODE),
                new Line(Language.BFPP, CODE),
                new Line(Language.BFPP, CODE),
                new Line(Language.BFPP, CODE),
                new Line(Language.BFPP, CODE),
        };
        assertLines(Language.BFPP, expected, code);
    }
}
