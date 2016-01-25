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
import static com.blackducksoftware.ohcount4j.Language.EBUILD;

import org.testng.annotations.Test;

public class EbuildScannerTest extends AbstractBaseScannerTest {

    @Test
    public void basic() {
        assertLine(EBUILD, new Line(EBUILD, BLANK), "\n");
        assertLine(EBUILD, new Line(EBUILD, BLANK), "     \n");
        assertLine(EBUILD, new Line(EBUILD, BLANK), "\t\n");
        assertLine(EBUILD, new Line(EBUILD, CODE), "EAPI=5\n");
        assertLine(EBUILD, new Line(EBUILD, COMMENT), "# Line comment\n");
        assertLine(EBUILD, new Line(EBUILD, COMMENT), "#\n");
        assertLine(EBUILD, new Line(EBUILD, CODE), "EAPI=6 # with comment\n");
    }

    @Test
    public void eofHandling() {
        // Note lack of trailing \n in all cases below
        assertLine(EBUILD, new Line(EBUILD, BLANK), "     ");
        assertLine(EBUILD, new Line(EBUILD, BLANK), "\t");
        assertLine(EBUILD, new Line(EBUILD, CODE), "LICENSE=\"GPL-2\"");
        assertLine(EBUILD, new Line(EBUILD, CODE), "    econf --with-popt");
        assertLine(EBUILD, new Line(EBUILD, COMMENT), "# sys-devel/flex");
        assertLine(EBUILD, new Line(EBUILD, COMMENT), "#");
        assertLine(EBUILD, new Line(EBUILD, CODE), "sys-devel/flex # with comment");
    }

    @Test
    public void simpleTest() {
        String code = "RC_URI=\"mirror://sourceforge/ctags/${P}.tar.gz\"\n"
                + "\n"
                + "LICENSE=\"GPL-2\"\n"
                + " \n"
                + "src_install() {\n"
                + "    emake DESTDIR=\"${D}\" install\n"
                + "\t\n"
                + "    dodoc FAQ NEWS README\n"
                + "    # comment 1\n"
                + "# comment 2\n"
                + "\t# comment 2\n"
                + "    dohtml EXTENDING.html ctags.html\n"
                + "}\n";

        Line[] expected = {
                new Line(EBUILD, CODE),
                new Line(EBUILD, BLANK),
                new Line(EBUILD, CODE),
                new Line(EBUILD, BLANK),
                new Line(EBUILD, CODE),
                new Line(EBUILD, CODE),
                new Line(EBUILD, BLANK),
                new Line(EBUILD, CODE),
                new Line(EBUILD, COMMENT),
                new Line(EBUILD, COMMENT),
                new Line(EBUILD, COMMENT),
                new Line(EBUILD, CODE),
                new Line(EBUILD, CODE),
        };
        assertLines(EBUILD, expected, code);
    }

}
