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

public class EiffelScannerTest extends AbstractBaseScannerTest {

    @Test
    public void basic() {
        assertLine(Language.EIFFEL, new Line(Language.EIFFEL, BLANK), "\n");
        assertLine(Language.EIFFEL, new Line(Language.EIFFEL, BLANK), "     \n");
        assertLine(Language.EIFFEL, new Line(Language.EIFFEL, BLANK), "\t\n");
        assertLine(Language.EIFFEL, new Line(Language.EIFFEL, CODE), "number := phone_book [\"JILL SMITH\"]\n");
        assertLine(Language.EIFFEL, new Line(Language.EIFFEL, COMMENT), "-- Line comment\n");
        assertLine(Language.EIFFEL, new Line(Language.EIFFEL, COMMENT), "--\n");
        assertLine(Language.EIFFEL, new Line(Language.EIFFEL, CODE), "number := phone_book [\"JILL SMITH\"] -- with comment\n");
    }

    @Test
    public void eofHandling() {
        // Note lack of trailing \n in all cases below
        assertLine(Language.EIFFEL, new Line(Language.EIFFEL, BLANK), "     ");
        assertLine(Language.EIFFEL, new Line(Language.EIFFEL, BLANK), "\t\n");
        assertLine(Language.EIFFEL, new Line(Language.EIFFEL, CODE), "number := phone_book [\"JILL SMITH\"]");
        assertLine(Language.EIFFEL, new Line(Language.EIFFEL, COMMENT), "-- Line comment");
        assertLine(Language.EIFFEL, new Line(Language.EIFFEL, COMMENT), "--");
        assertLine(Language.EIFFEL, new Line(Language.EIFFEL, CODE), "number := phone_book [\"JILL SMITH\"] -- with comment");
    }

    @Test
    public void helloWorld() {
        String code = "-- Hello World Eiffel Program\n"
                + "class HELLO_WORLD\n"
                + "\n"
                + "creation\n"
                + "		make\n"
                + "feature\n"
                + "		make is\n"
                + "		local\n"
                + "    		io:BASIC_IO\n"
                + "		do\n"
                + "		    !!io\n"
                + "			io.put_string(\"%N Hello World!!!!\")\n"
                + "		end --make\n"
                + "\n"
                + "end -- class HELLO_WORLD";

        Line[] expected = {
                new Line(Language.EIFFEL, COMMENT),
                new Line(Language.EIFFEL, CODE),
                new Line(Language.EIFFEL, BLANK),
                new Line(Language.EIFFEL, CODE),
                new Line(Language.EIFFEL, CODE),
                new Line(Language.EIFFEL, CODE),
                new Line(Language.EIFFEL, CODE),
                new Line(Language.EIFFEL, CODE),
                new Line(Language.EIFFEL, CODE),
                new Line(Language.EIFFEL, CODE),
                new Line(Language.EIFFEL, CODE),
                new Line(Language.EIFFEL, CODE),
                new Line(Language.EIFFEL, CODE),
                new Line(Language.EIFFEL, BLANK),
                new Line(Language.EIFFEL, CODE)
        };
        assertLines(Language.EIFFEL, expected, code);
    }

}
