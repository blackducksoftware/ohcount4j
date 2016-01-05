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

public class OCamlScannerTest extends AbstractBaseScannerTest {

    @Test
    public void basic() {
        assertLine(Language.OCAML, new Line(Language.OCAML, BLANK), "\n");
        assertLine(Language.OCAML, new Line(Language.OCAML, BLANK), "     \n");
        assertLine(Language.OCAML, new Line(Language.OCAML, BLANK), "\t\n");
        assertLine(Language.OCAML, new Line(Language.OCAML, CODE), "let rec qsort = function\n");
        assertLine(Language.OCAML, new Line(Language.OCAML, COMMENT), "(* Block Comment *)\n");
        assertLine(Language.OCAML, new Line(Language.OCAML, COMMENT), "(**)\n");
        assertLine(Language.OCAML, new Line(Language.OCAML, CODE), "let rec qsort = function (* with comment *)\n");
    }

    @Test
    public void eofHandling() {
        // Note lack of trailing \n in all cases below
        assertLine(Language.OCAML, new Line(Language.OCAML, BLANK), "     ");
        assertLine(Language.OCAML, new Line(Language.OCAML, BLANK), "\t");
        assertLine(Language.OCAML, new Line(Language.OCAML, CODE), "let rec qsort = function");
        assertLine(Language.OCAML, new Line(Language.OCAML, COMMENT), "(* Block Comment *)");
        assertLine(Language.OCAML, new Line(Language.OCAML, COMMENT), "(**)");
        assertLine(Language.OCAML, new Line(Language.OCAML, CODE), "let rec qsort = function (* with comment *)");
    }

    @Test
    public void sampleTest() {
        String code = "(* That is a comment (* and this is a comment inside a comment\n"
                + "\n"
                + "*) continuing on several lines *)\n"
                + "\t\n"
                + "\"Some String \" (* With comment inside *) but with escaped end quote\"\n"
                + "\"Some String\" (* comment after *)\n"
                + "\n"
                + "let rec quicksort = function (* Quicksort impl in OCaml *)\n"
                + "		| [] -> []\n"
                + "		| pivot :: rest ->\n"
                + "			let is_less x = x < pivot in\n"
                + "			let left, right = List.partition is_less rest in\n"
                + "			quicksort left @ [pivot] @ quicksort right\n";

        Line[] expected = {
                new Line(Language.OCAML, COMMENT),
                new Line(Language.OCAML, BLANK),
                new Line(Language.OCAML, COMMENT),
                new Line(Language.OCAML, BLANK),
                new Line(Language.OCAML, CODE),
                new Line(Language.OCAML, CODE),
                new Line(Language.OCAML, BLANK),
                new Line(Language.OCAML, CODE),
                new Line(Language.OCAML, CODE),
                new Line(Language.OCAML, CODE),
                new Line(Language.OCAML, CODE),
                new Line(Language.OCAML, CODE),
                new Line(Language.OCAML, CODE)
        };
        assertLines(Language.OCAML, expected, code);
    }

    @Test
    public void unterminatedNestedCommentCrash() {
        // This minimal case caused an Arrays.copyOfRange() crash
        String code = "(*\n(*\n*)\n\n\n";

        Line[] expected = {
                new Line(Language.OCAML, COMMENT),
                new Line(Language.OCAML, COMMENT),
                new Line(Language.OCAML, COMMENT),
                new Line(Language.OCAML, BLANK),
                new Line(Language.OCAML, BLANK)
        };
        assertLines(Language.OCAML, expected, code);
    }
}
