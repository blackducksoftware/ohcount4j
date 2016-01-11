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
package com.blackducksoftware.ohcount4j;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang3.mutable.MutableInt;

import com.blackducksoftware.ohcount4j.detect.Detector;
import com.blackducksoftware.ohcount4j.scan.Line;
import com.blackducksoftware.ohcount4j.scan.LineHandler;

import difflib.Chunk;
import difflib.Delta;
import difflib.Delta.TYPE;
import difflib.DiffUtils;
import difflib.Patch;

/**
 * @author mpujari
 *
 */
public class DiffSourceFile {

    /**
     * Returns the list of {@link LanguageDiff}s for the difference.
     *
     * @param from
     *            the previous file
     * @param to
     *            the changed file
     * @return
     *         the list of {@link LanguageDiff}s for the difference.
     * @throws IOException
     */
    public List<LanguageDiff> diff(SourceFile from, SourceFile to) throws IOException {
        List<LanguageDiff> languageDiffs = new ArrayList<>();
        if (from == null && to == null) {
            return languageDiffs;
        }

        // If "from" sourceFile is null, compare "to" with empty contents with same fileName.
        // This happens when a file is newly ADDED.
        if (from == null) {
            from = new SourceFile(to.getPath(), "");
        }

        // If "to" sourceFile is null, compare "from" with empty contents with same fileName.
        // This happens when a file is DELETED.
        if (to == null) {
            to = new SourceFile(from.getPath(), "");
        }

        Language fromLanguage = Detector.detect(from);
        Language toLanguage = Detector.detect(to);

        LineDetailHandler fromLinehandler = new LineDetailHandler();
        fromLanguage.makeScanner().scan(from, fromLinehandler);

        LineDetailHandler toLinehandler = new LineDetailHandler();
        toLanguage.makeScanner().scan(to, toLinehandler);

        List<String> original = fromLinehandler.contentList;
        List<String> revised = toLinehandler.contentList;

        // Compute diff. Get the Patch object. Patch is the container for computed deltas.
        Patch<String> patch = DiffUtils.diff(original, revised);

        Map<Language, MutableInt[]> languageMap = new HashMap<>();
        for (Delta<String> delta : patch.getDeltas()) {
            switch (delta.getType()) {
            case CHANGE: {
                // its considered as 1 is added and 1 is removed
                accountDiff(languageMap, Delta.TYPE.DELETE, delta.getOriginal(), fromLinehandler);
                accountDiff(languageMap, Delta.TYPE.INSERT, delta.getRevised(), toLinehandler);
                break;
            }
            case DELETE: {
                accountDiff(languageMap, Delta.TYPE.DELETE, delta.getOriginal(), fromLinehandler);
                break;
            }
            case INSERT: {
                accountDiff(languageMap, Delta.TYPE.INSERT, delta.getRevised(), toLinehandler);
                break;
            }
            }
        }

        for (Map.Entry<Language, MutableInt[]> element : languageMap.entrySet()) {
            MutableInt[] value = element.getValue();
            languageDiffs.add(new LanguageDiff(element.getKey(),
                    value[0].intValue(), // Code Added
                    value[1].intValue(), // Code Removed
                    value[2].intValue(), // Comments Added
                    value[3].intValue(), // Comments Removed
                    value[4].intValue(), // Blanks Added
                    value[5].intValue())); // Blanks Removed
        }

        return languageDiffs;
    }

    private void accountDiff(Map<Language, MutableInt[]> languageMap, TYPE type, Chunk<String> chunk, LineDetailHandler linehandler) {
        if (type == TYPE.CHANGE) {
            // CHANGE should be like INSERT and DELETE
            throw new IllegalArgumentException("TYPE.CHANGE should not be sent");
        }
        int position = chunk.getPosition();
        for (int i = 0; i < chunk.getLines().size(); i++) {
            Line line = linehandler.lineDetails.get(position + i);

            // Check if the language is already detected. If no, create new array to hold the diff.
            MutableInt[] diff = languageMap.get(line.getLanguage());
            if (diff == null) {
                diff = defaultDiff();
                languageMap.put(line.getLanguage(), diff);
            }

            switch (line.getEntity()) {
            case CODE:
                if (type == TYPE.INSERT) {
                    diff[0].increment();// Code Added
                } else {
                    diff[1].increment(); // Code removed
                }
                break;
            case COMMENT:
                if (type == TYPE.INSERT) {
                    diff[2].increment();// Comments Added
                } else {
                    diff[3].increment();// Comments Removed
                }
                break;
            case BLANK:
                if (type == TYPE.INSERT) {
                    diff[4].increment(); // Blanks Added
                } else {
                    diff[5].increment();// Blanks Removed
                }
                break;
            }
        }
    }

    private MutableInt[] defaultDiff() {
        // Return preinitialized array of MutableInt.
        return new MutableInt[] { new MutableInt(0), new MutableInt(0), new MutableInt(0), new MutableInt(0),
                new MutableInt(0), new MutableInt(0), };
    }

    private static class LineDetailHandler implements LineHandler {

        List<Line> lineDetails = new ArrayList<Line>();

        List<String> contentList = new ArrayList<String>();

        @Override
        public void handleLine(Line line) {
            lineDetails.add(line);
            contentList.add(line.getContent());
        }

    }

}
