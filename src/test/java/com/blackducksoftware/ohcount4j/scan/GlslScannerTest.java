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

import java.io.File;

import org.testng.annotations.Test;

import com.blackducksoftware.ohcount4j.Language;
import com.blackducksoftware.ohcount4j.SourceFile;

public class GlslScannerTest extends BaseScannerTest {

    @Test
    public void basic() {
        assertLine(Language.GLSL, new Line(Language.GLSL, BLANK), "\n");
        assertLine(Language.GLSL, new Line(Language.GLSL, BLANK), "     \n");
        assertLine(Language.GLSL, new Line(Language.GLSL, BLANK), "\t\n");
        assertLine(Language.GLSL, new Line(Language.GLSL, CODE), "varying vec3 normal;\n");
        assertLine(Language.GLSL, new Line(Language.GLSL, CODE), "void main(){\n");
        assertLine(Language.GLSL, new Line(Language.GLSL, COMMENT), "// Line comment\n");
        assertLine(Language.GLSL, new Line(Language.GLSL, COMMENT), "/* Block Comment */\n");
        assertLine(Language.GLSL, new Line(Language.GLSL, CODE), "const vec4 DiffuseColor = vec4(1.0, 0.0, 0.0, 1.0);\n");
        assertLine(Language.GLSL, new Line(Language.GLSL, CODE), "gl_FragColor = AmbientColor + DiffuseColor * DiffuseTerm;\n");
        assertLine(Language.GLSL, new Line(Language.GLSL, CODE), "texture_coordinate = vec2(gl_MultiTexCoord0);\n");
    }

    @Test
    public void helloWorld() {
        String code = "/* Hello World\n"
                + " * with multi-line comment */\n"
                + "\n"
                + "varying vec3 normal;\n"
                + "\n"
                + "void main(){\n"
                + "// single line comment\n"
                + "\t\tvTrans = projection * modelview * incomingVertex;\n"
                + "\t\tuniform mat4 gl_ModelViewMatrix;\n"
                + "\t}\n";
        Line[] expected = {
                new Line(Language.GLSL, COMMENT),
                new Line(Language.GLSL, COMMENT),
                new Line(Language.GLSL, BLANK),
                new Line(Language.GLSL, CODE),
                new Line(Language.GLSL, BLANK),
                new Line(Language.GLSL, CODE),
                new Line(Language.GLSL, COMMENT),
                new Line(Language.GLSL, CODE),
                new Line(Language.GLSL, CODE),
                new Line(Language.GLSL, CODE),
        };
        assertLines(Language.GLSL, expected, code);
    }

    @Test
    public void eofHandling() {
        // Note lack of trailing \n in all cases below
        assertLine(Language.GLSL, new Line(Language.GLSL, BLANK), "     ");
        assertLine(Language.GLSL, new Line(Language.GLSL, BLANK), "\t");
        assertLine(Language.GLSL, new Line(Language.GLSL, CODE), "varying vec3 normal;");
        assertLine(Language.GLSL, new Line(Language.GLSL, CODE), "void main(){");
        assertLine(Language.GLSL, new Line(Language.GLSL, COMMENT), "// Line comment");
        assertLine(Language.GLSL, new Line(Language.GLSL, COMMENT), "/* Block Comment */");
        assertLine(Language.GLSL, new Line(Language.GLSL, CODE), "const vec4 DiffuseColor = vec4(1.0, 0.0, 0.0, 1.0);");
        assertLine(Language.GLSL, new Line(Language.GLSL, CODE), "gl_FragColor = AmbientColor + DiffuseColor * DiffuseTerm;");
        assertLine(Language.GLSL, new Line(Language.GLSL, CODE), "texture_coordinate = vec2(gl_MultiTexCoord0);");
    }

    @Test
    public void sampleTestWithSourceFile() throws Exception {
        SourceFile sourceFile = new SourceFile(new File(getSourceCodePath("foo_glsl.vert")));
        Line[] expected = {
                new Line(Language.GLSL, COMMENT),
                new Line(Language.GLSL, COMMENT),
                new Line(Language.GLSL, BLANK),
                new Line(Language.GLSL, CODE),
                new Line(Language.GLSL, CODE),
                new Line(Language.GLSL, CODE),
                new Line(Language.GLSL, CODE),
                new Line(Language.GLSL, BLANK),
                new Line(Language.GLSL, COMMENT),
                new Line(Language.GLSL, COMMENT),
                new Line(Language.GLSL, COMMENT),
                new Line(Language.GLSL, CODE),
                new Line(Language.GLSL, CODE),
                new Line(Language.GLSL, CODE),
                new Line(Language.GLSL, BLANK),
                new Line(Language.GLSL, CODE),
                new Line(Language.GLSL, CODE),
                new Line(Language.GLSL, CODE),
                new Line(Language.GLSL, BLANK),
                new Line(Language.GLSL, CODE),
                new Line(Language.GLSL, CODE),
                new Line(Language.GLSL, COMMENT),
                new Line(Language.GLSL, COMMENT),
                new Line(Language.GLSL, CODE),
                new Line(Language.GLSL, COMMENT),
                new Line(Language.GLSL, COMMENT),
                new Line(Language.GLSL, COMMENT),
                new Line(Language.GLSL, CODE),
                new Line(Language.GLSL, CODE),
                new Line(Language.GLSL, CODE),
                new Line(Language.GLSL, CODE),
                new Line(Language.GLSL, CODE),
                new Line(Language.GLSL, COMMENT),
                new Line(Language.GLSL, CODE),
                new Line(Language.GLSL, CODE),
                new Line(Language.GLSL, BLANK),
                new Line(Language.GLSL, COMMENT),
                new Line(Language.GLSL, CODE),
                new Line(Language.GLSL, BLANK),
                new Line(Language.GLSL, CODE),
                new Line(Language.GLSL, CODE),
                new Line(Language.GLSL, CODE),
                new Line(Language.GLSL, CODE),
        };
        assertLines(Language.GLSL, expected, sourceFile);
    }

}
