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
package net.ohloh.ohcount4j.scan;

import static net.ohloh.ohcount4j.Entity.BLANK;
import static net.ohloh.ohcount4j.Entity.CODE;
import static net.ohloh.ohcount4j.Entity.COMMENT;
import net.ohloh.ohcount4j.Language;

import org.testng.annotations.Test;

/**
 * @author gandhip
 *
 */
public class BfkScannerTest extends AbstractBaseScannerTest {

    @Test
    public void basic() {
        assertLine(Language.BRAINFUCK, new Line(Language.BRAINFUCK, BLANK), "\n");
        assertLine(Language.BRAINFUCK, new Line(Language.BRAINFUCK, BLANK), "     \n");
        assertLine(Language.BRAINFUCK, new Line(Language.BRAINFUCK, BLANK), "\t\n");
        assertLine(Language.BRAINFUCK, new Line(Language.BRAINFUCK, CODE), ">++++++[>+++[<<++++>>-]<-]<.\n");
        assertLine(Language.BRAINFUCK, new Line(Language.BRAINFUCK, CODE), "+++++++.\n");
        assertLine(Language.BRAINFUCK, new Line(Language.BRAINFUCK, COMMENT), "Line that does nothing:\n");
    }

    @Test
    public void eofHandling() {
        // Note lack of trailing \n in all cases below
        assertLine(Language.BRAINFUCK, new Line(Language.BRAINFUCK, BLANK), "     ");
        assertLine(Language.BRAINFUCK, new Line(Language.BRAINFUCK, BLANK), "\t");
        assertLine(Language.BRAINFUCK, new Line(Language.BRAINFUCK, CODE), ">++++++[>+++[<<++++>>-]<-]<.");
        assertLine(Language.BRAINFUCK, new Line(Language.BRAINFUCK, CODE), "+++++++.");
        assertLine(Language.BRAINFUCK, new Line(Language.BRAINFUCK, COMMENT), "Line that does nothing:");
    }

    @Test
    public void testHelloWorlde() throws Exception {
        String code = "Print \"Hello World!!!!!\"\n"
                + "\n"
                + "Line that does nothing: ><\n"
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
                + ">>++[<+++++>-]<+[<------>-]<-.\n"
                + ".\n"
                + ".\n"
                + ".\n"
                + ".";
        Line[] expected = {
                new Line(Language.BRAINFUCK, COMMENT),
                new Line(Language.BRAINFUCK, BLANK),
                new Line(Language.BRAINFUCK, CODE),
                new Line(Language.BRAINFUCK, CODE),
                new Line(Language.BRAINFUCK, CODE),
                new Line(Language.BRAINFUCK, CODE),
                new Line(Language.BRAINFUCK, CODE),
                new Line(Language.BRAINFUCK, CODE),
                new Line(Language.BRAINFUCK, CODE),
                new Line(Language.BRAINFUCK, CODE),
                new Line(Language.BRAINFUCK, CODE),
                new Line(Language.BRAINFUCK, CODE),
                new Line(Language.BRAINFUCK, CODE),
                new Line(Language.BRAINFUCK, CODE),
                new Line(Language.BRAINFUCK, CODE),
                new Line(Language.BRAINFUCK, CODE),
                new Line(Language.BRAINFUCK, CODE),
                new Line(Language.BRAINFUCK, CODE),
                new Line(Language.BRAINFUCK, CODE),
        };
        assertLines(Language.BRAINFUCK, expected, code);
    }

}
