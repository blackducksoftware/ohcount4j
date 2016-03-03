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
import static com.blackducksoftware.ohcount4j.Language.COQ;

import org.testng.annotations.Test;

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
