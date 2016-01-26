/*
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

import org.testng.annotations.Test;

import com.blackducksoftware.ohcount4j.Language;

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
