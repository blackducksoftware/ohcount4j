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

package com.blackducksoftware.ohcount4j.detect;

import static com.blackducksoftware.ohcount4j.Language.C;
import static com.blackducksoftware.ohcount4j.Language.CPP;
import static com.blackducksoftware.ohcount4j.Language.OBJECTIVE_C;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.blackducksoftware.ohcount4j.Language;
import com.blackducksoftware.ohcount4j.SourceFile;
import com.blackducksoftware.ohcount4j.SourceFileUtils;

public class ExtnHResolver extends AbstractExtnResolver {

    private static final Pattern CPP_KEYWORDS_PATTERN = Pattern.compile("\\b(?:class|namespace|template|typename)\\b", Pattern.MULTILINE);

    private static final Pattern INCLUDE_PATTERN = Pattern.compile("^#include\\s*(?:<|\")([\\w\\.\\/]+)(?:>|\")", Pattern.MULTILINE);

    @Override
    public Language resolve(SourceFile sourceFile, List<String> filenames) throws IOException {
        Language result = null;

        if (result == null) {
            result = resolveByNearbyFiles(sourceFile, filenames);
        }
        if (result == null) {
            result = resolveByIncludes(sourceFile);
        }
        if (result == null) {
            result = resolveByKeywords(sourceFile);
        }
        if (result == null) {
            result = Language.C;
        }

        return result;
    }

    @Override
    public Language resolve(SourceFile sourceFile) throws IOException {
        return resolve(sourceFile, new ArrayList<String>());
    }

    @Override
    public boolean canResolve(Language language) {
        if (language == C || language == CPP || language == OBJECTIVE_C) {
            return true;
        }
        return false;
    }

    /*
     * Does the source tree contain a *.m file to match this *.h file?
     * If so, likely Objective-C.
     */
    private Language resolveByNearbyFiles(SourceFile source, List<String> filenames) {

        String path_with_m = source.getPath().replaceFirst("\\.h$", ".m");

        if (filenames.contains(path_with_m)) {
            return Language.OBJECTIVE_C;
        } else {
            return null;
        }
    }

    private Language resolveByKeywords(SourceFile source) throws IOException {
        Matcher m = CPP_KEYWORDS_PATTERN.matcher(new String(SourceFileUtils.getContents(source)));
        if (m.find()) {
            return Language.CPP;
        } else {
            return null;
        }
    }

    /*
     * Returns the names of all libraries #included in this file
     */
    public List<String> findIncludes(SourceFile source) throws IOException {
        ArrayList<String> result = new ArrayList<String>();
        Matcher m = INCLUDE_PATTERN.matcher(new String(SourceFileUtils.getContents(source)));
        while (m.find()) {
            result.add(m.group(1));
        }
        return result;
    }

    /*
     * Look for headers which are used by C++ only
     */
    private Language resolveByIncludes(SourceFile source) throws IOException {
        for (String include : findIncludes(source)) {
            if (CPP_INCLUDES.contains(include)) {
                return Language.CPP;
            }
        }
        return null;
    }

    @SuppressWarnings("serial")
    private static final Set<String> CPP_INCLUDES = new HashSet<String>() {
        {
            add("string");
            add("algorithm");
            add("array");
            add("bitset");
            add("cassert");
            add("ccomplex");
            add("cctype");
            add("cerrno");
            add("cfenv");
            add("cfloat");
            add("cinttypes");
            add("ciso646");
            add("climits");
            add("clocale");
            add("cmath");
            add("csetjmp");
            add("csignal");
            add("cstdarg");
            add("cstdbool");
            add("cstddef");
            add("cstdint");
            add("cstdio");
            add("cstdlib");
            add("cstring");
            add("ctgmath");
            add("ctime");
            add("cwchar");
            add("cwctype");
            add("deque");
            add("exception");
            add("fstream");
            add("functional");
            add("iomanip");
            add("ios");
            add("iosfwd");
            add("iostream");
            add("istream");
            add("iterator");
            add("limits");
            add("list");
            add("locale");
            add("map");
            add("memory");
            add("new");
            add("numeric");
            add("ostream");
            add("queue");
            add("random");
            add("regex");
            add("set");
            add("sstream");
            add("stack");
            add("stdexcept");
            add("streambuf");
            add("string");
            add("system_error");
            add("tuple");
            add("type_traits");
            add("typeinfo");
            add("unordered_map");
            add("unordered_set");
            add("utility");
            add("valarray");
            add("vector");
            add("tr1/array");
            add("tr1/ccomplex");
            add("tr1/cctype");
            add("tr1/cfenv");
            add("tr1/cfloat");
            add("tr1/cinttypes");
            add("tr1/climits");
            add("tr1/cmath");
            add("tr1/complex");
            add("tr1/cstdarg");
            add("tr1/cstdbool");
            add("tr1/cstdint");
            add("tr1/cstdio");
            add("tr1/cstdlib");
            add("tr1/ctgmath");
            add("tr1/ctime");
            add("tr1/cwchar");
            add("tr1/cwctype");
            add("tr1/memory");
            add("tr1/random");
            add("tr1/regex");
            add("tr1/tuple");
            add("tr1/type_traits");
            add("tr1/unordered_map");
            add("tr1/unordered_set");
            add("tr1/utility");
        }
    };
}
