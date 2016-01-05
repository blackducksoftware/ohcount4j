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

public class LispScannerTest extends AbstractBaseScannerTest {

    @Test
    public void basic() {
        assertLine(Language.LISP, new Line(Language.LISP, BLANK), "\n");
        assertLine(Language.LISP, new Line(Language.LISP, BLANK), "     \n");
        assertLine(Language.LISP, new Line(Language.LISP, BLANK), "\t\n");
        assertLine(Language.LISP, new Line(Language.LISP, CODE), "((lambda (arg) (+ arg 1)) 5)\n");
        assertLine(Language.LISP, new Line(Language.LISP, COMMENT), "#| Block Comment |#\n");
        assertLine(Language.LISP, new Line(Language.LISP, COMMENT), "; Line comment\n");
        assertLine(Language.LISP, new Line(Language.LISP, COMMENT), ";\n");
        assertLine(Language.LISP, new Line(Language.LISP, CODE), "((lambda (arg) (+ arg 1)) 5) ; with comment\n");
    }

    @Test
    public void eofHandling() {
        // Note lack of trailing \n in all cases below
        assertLine(Language.LISP, new Line(Language.LISP, BLANK), "     ");
        assertLine(Language.LISP, new Line(Language.LISP, BLANK), "\t");
        assertLine(Language.LISP, new Line(Language.LISP, CODE), "((lambda (arg) (+ arg 1)) 5)");
        assertLine(Language.LISP, new Line(Language.LISP, COMMENT), "#| Block Comment |#");
        assertLine(Language.LISP, new Line(Language.LISP, COMMENT), "; Line comment");
        assertLine(Language.LISP, new Line(Language.LISP, COMMENT), ";");
        assertLine(Language.LISP, new Line(Language.LISP, CODE), "((lambda (arg) (+ arg 1)) 5) ; with comment");
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
                new Line(Language.LISP, COMMENT),
                new Line(Language.LISP, COMMENT),
                new Line(Language.LISP, BLANK),
                new Line(Language.LISP, COMMENT),
                new Line(Language.LISP, BLANK),
                new Line(Language.LISP, CODE),
                new Line(Language.LISP, COMMENT),
                new Line(Language.LISP, COMMENT),
                new Line(Language.LISP, CODE),
                new Line(Language.LISP, CODE),
                new Line(Language.LISP, CODE),
                new Line(Language.LISP, COMMENT)
        };
        assertLines(Language.LISP, expected, code);
    }

    @Test
    public void unterminatedBlockCommentCrash() {
        // This minimal case caused an Arrays.copyOfRange() crash
        String code = "#|\n\n\n";

        Line[] expected = {
                new Line(Language.LISP, COMMENT),
                new Line(Language.LISP, BLANK),
                new Line(Language.LISP, BLANK)
        };
        assertLines(Language.LISP, expected, code);
    }

}
