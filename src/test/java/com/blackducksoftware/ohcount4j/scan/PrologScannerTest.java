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

public class PrologScannerTest extends AbstractBaseScannerTest {

    @Test
    public void basic() {
        assertLine(Language.PROLOG, new Line(Language.PROLOG, BLANK), "\n");
        assertLine(Language.PROLOG, new Line(Language.PROLOG, BLANK), "     \n");
        assertLine(Language.PROLOG, new Line(Language.PROLOG, BLANK), "\t\n");
        assertLine(Language.PROLOG, new Line(Language.PROLOG, CODE), "hello_world :- write('Hello World!').\n");
        assertLine(Language.PROLOG, new Line(Language.PROLOG, COMMENT), "/* Block Comment */\n");
        assertLine(Language.PROLOG, new Line(Language.PROLOG, COMMENT), "% Line comment\n");
        assertLine(Language.PROLOG, new Line(Language.PROLOG, COMMENT), "%\n");
        assertLine(Language.PROLOG, new Line(Language.PROLOG, CODE), "hello_world :- write('Hello World!'). // with comment\n");
    }

    @Test
    public void eofHandling() {
        // Note lack of trailing \n in all cases below
        assertLine(Language.PROLOG, new Line(Language.PROLOG, BLANK), "     ");
        assertLine(Language.PROLOG, new Line(Language.PROLOG, BLANK), "\t");
        assertLine(Language.PROLOG, new Line(Language.PROLOG, CODE), "hello_world :- write('Hello World!').");
        assertLine(Language.PROLOG, new Line(Language.PROLOG, COMMENT), "/* Block Comment */");
        assertLine(Language.PROLOG, new Line(Language.PROLOG, COMMENT), "% Line comment");
        assertLine(Language.PROLOG, new Line(Language.PROLOG, COMMENT), "%");
        assertLine(Language.PROLOG, new Line(Language.PROLOG, CODE), "hello_world :- write('Hello World!'). // with comment");
    }

    @Test
    public void sampleTest() {
        String code = "/* QuickSort\n"
                + " * Written in Prolog\n"
                + "\n"
                + " */\n"
                + "partition([], _, [], []).\n"
                + "partition([X|Xs], Pivot, Smalls, Bigs) :-\n"
                + "		(   X @< Pivot ->\n"
                + "			Smalls = [X|Rest],\n"
                + "			partition(Xs, Pivot, Rest, Bigs)\n"
                + "		;   Bigs = [X|Rest],\n"
                + "			partition(Xs, Pivot, Smalls, Rest)\n"
                + "		).\n"
                + "\t\n"
                + "quicksort([])     --> [].\n"
                + "quicksort([X|Xs]) -->\n"
                + "		{ partition(Xs, X, Smaller, Bigger) },\n"
                + "		quicksort(Smaller), [X], quicksort(Bigger).\n";

        Line[] expected = {
                new Line(Language.PROLOG, COMMENT),
                new Line(Language.PROLOG, COMMENT),
                new Line(Language.PROLOG, BLANK),
                new Line(Language.PROLOG, COMMENT),
                new Line(Language.PROLOG, CODE),
                new Line(Language.PROLOG, CODE),
                new Line(Language.PROLOG, CODE),
                new Line(Language.PROLOG, CODE),
                new Line(Language.PROLOG, CODE),
                new Line(Language.PROLOG, CODE),
                new Line(Language.PROLOG, CODE),
                new Line(Language.PROLOG, CODE),
                new Line(Language.PROLOG, BLANK),
                new Line(Language.PROLOG, CODE),
                new Line(Language.PROLOG, CODE),
                new Line(Language.PROLOG, CODE),
                new Line(Language.PROLOG, CODE)
        };
        assertLines(Language.PROLOG, expected, code);
    }

}
