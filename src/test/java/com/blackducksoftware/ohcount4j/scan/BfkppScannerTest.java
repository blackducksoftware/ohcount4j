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
