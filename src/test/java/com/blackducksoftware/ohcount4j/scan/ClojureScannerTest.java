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
import static com.blackducksoftware.ohcount4j.Language.CLOJURE;

import org.testng.annotations.Test;

public class ClojureScannerTest extends AbstractBaseScannerTest {

    @Test
    public void basic() {
        assertLine(CLOJURE, new Line(CLOJURE, BLANK), "\n");
        assertLine(CLOJURE, new Line(CLOJURE, BLANK), "     \n");
        assertLine(CLOJURE, new Line(CLOJURE, BLANK), "\t\n");
        assertLine(CLOJURE, new Line(CLOJURE, CODE), "(+ 1 2 3)\n");
        assertLine(CLOJURE, new Line(CLOJURE, COMMENT), "; Comment\n");
        assertLine(CLOJURE, new Line(CLOJURE, COMMENT), ";; Comment\n");
        assertLine(CLOJURE, new Line(CLOJURE, COMMENT), ";;; Comment\n");
        assertLine(CLOJURE, new Line(CLOJURE, CODE), "#_(a : b) 1  ;;=> 1 (contrast to (comment a : b) which doesn't compile.)\n");
    }

    @Test
    public void eofHandling() {
        // Note lack of trailing \n in all cases below
        assertLine(CLOJURE, new Line(CLOJURE, BLANK), "     ");
        assertLine(CLOJURE, new Line(CLOJURE, BLANK), "\t");
        assertLine(CLOJURE, new Line(CLOJURE, CODE), "(ns com.clojurebook.broken-array-set)");
        assertLine(CLOJURE, new Line(CLOJURE, COMMENT), "; Comment");
        assertLine(CLOJURE, new Line(CLOJURE, COMMENT), ";; Comment");
        assertLine(CLOJURE, new Line(CLOJURE, COMMENT), ";;; Comment");
        assertLine(CLOJURE, new Line(CLOJURE, CODE), "(println (\"Hello World!\"))");
    }

    @Test
    public void helloWorld() {
        String code = "(defn array-set\n"
                + "  ; comment\n"
                + "  ;; comment\n"
                + "  ;;; comment\n"
                + "\n"
                + "  \"Creates an array-backed set containing the given values.\"\n"
                + "  [& vals]\n"
                + "  (into empty-array-set vals))";

        Line[] expected = {
                new Line(CLOJURE, CODE),
                new Line(CLOJURE, COMMENT),
                new Line(CLOJURE, COMMENT),
                new Line(CLOJURE, COMMENT),
                new Line(CLOJURE, BLANK),
                new Line(CLOJURE, CODE),
                new Line(CLOJURE, CODE),
                new Line(CLOJURE, CODE),
        };
        assertLines(CLOJURE, expected, code);
    }

}
