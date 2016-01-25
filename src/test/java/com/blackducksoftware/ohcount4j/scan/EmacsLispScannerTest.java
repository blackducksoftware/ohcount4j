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

public class EmacsLispScannerTest extends AbstractBaseScannerTest {

    @Test
    public void basic() {
        assertLine(Language.EMACSLISP, new Line(Language.EMACSLISP, BLANK), "\n");
        assertLine(Language.EMACSLISP, new Line(Language.EMACSLISP, BLANK), "     \n");
        assertLine(Language.EMACSLISP, new Line(Language.EMACSLISP, BLANK), "\t\n");
        assertLine(Language.EMACSLISP, new Line(Language.EMACSLISP, CODE), "(vector 1 2)\n");
        assertLine(Language.EMACSLISP, new Line(Language.EMACSLISP, COMMENT), "#| Block Comment |#\n");
        assertLine(Language.EMACSLISP, new Line(Language.EMACSLISP, COMMENT), "; Line comment\n");
        assertLine(Language.EMACSLISP, new Line(Language.EMACSLISP, COMMENT), ";\n");
        assertLine(Language.EMACSLISP, new Line(Language.EMACSLISP, CODE), "(vector 1 2) ; => [1 2]\n");
    }

    @Test
    public void eofHandling() {
        // Note lack of trailing \n in all cases below
        assertLine(Language.EMACSLISP, new Line(Language.EMACSLISP, BLANK), "     ");
        assertLine(Language.EMACSLISP, new Line(Language.EMACSLISP, BLANK), "\t");
        assertLine(Language.EMACSLISP, new Line(Language.EMACSLISP, CODE), "(defun my-insert-stuff ())");
        assertLine(Language.EMACSLISP, new Line(Language.EMACSLISP, COMMENT), "#| Block Comment |#");
        assertLine(Language.EMACSLISP, new Line(Language.EMACSLISP, COMMENT), "; Line comment");
        assertLine(Language.EMACSLISP, new Line(Language.EMACSLISP, COMMENT), ";");
        assertLine(Language.EMACSLISP, new Line(Language.EMACSLISP, CODE), "(vector 1 2) ; => [1 2]");
    }

    @Test
    public void sampleTest() {
        String code = ";;; This function simply returns the string Hello World that is in quotes.\n"
                + "#| Rarely used but, \n"
                + "   \n"
                + "   multi-line comments can exist in this form |#\n"
                + "\t\n"
                + "(DEFUN HELLO ()\n"
                + "		\"Hello world\n"
                + "					documentation string\"\n"
                + "		\"MULTI-LINE~\n"
                + "		;STRING WITH SEMI-COLON\"\n"
                + ")\n"
                + ";";

        Line[] expected = {
                new Line(Language.EMACSLISP, COMMENT),
                new Line(Language.EMACSLISP, COMMENT),
                new Line(Language.EMACSLISP, BLANK),
                new Line(Language.EMACSLISP, COMMENT),
                new Line(Language.EMACSLISP, BLANK),
                new Line(Language.EMACSLISP, CODE),
                new Line(Language.EMACSLISP, COMMENT),
                new Line(Language.EMACSLISP, COMMENT),
                new Line(Language.EMACSLISP, CODE),
                new Line(Language.EMACSLISP, CODE),
                new Line(Language.EMACSLISP, CODE),
                new Line(Language.EMACSLISP, COMMENT)
        };
        assertLines(Language.EMACSLISP, expected, code);
    }

    @Test
    public void unterminatedBlockCommentCrash() {
        // This minimal case caused an Arrays.copyOfRange() crash
        String code = "#|\n\n\n";

        Line[] expected = {
                new Line(Language.EMACSLISP, COMMENT),
                new Line(Language.EMACSLISP, BLANK),
                new Line(Language.EMACSLISP, BLANK)
        };
        assertLines(Language.EMACSLISP, expected, code);
    }

}
