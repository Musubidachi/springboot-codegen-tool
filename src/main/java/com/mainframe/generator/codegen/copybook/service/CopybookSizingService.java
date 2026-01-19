package com.mainframe.generator.codegen.copybook.service;

import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Set;

import com.mainframe.generator.codegen.copybook.util.PictureClause;
import com.mainframe.generator.codegen.model.input.CopybookNode;
import com.mainframe.generator.codegen.model.input.FieldNode;
import com.mainframe.generator.codegen.model.input.GroupNode;
import com.mainframe.generator.codegen.model.input.UsageType;

import lombok.NoArgsConstructor;

@NoArgsConstructor
public class CopybookSizingService {

	/**
	 * Byte length for a single field occurrence (does NOT apply OCCURS).
	 */
	public int calculateByteLength(PictureClause picture, UsageType usage) {
		UsageType effective = (usage == null) ? UsageType.DISPLAY : usage;
		if (picture == null) {
			return switch (effective) {
			case COMP_1 -> 4;
			case COMP_2 -> 8;
			case COMP_5 -> 4;
			case BINARY -> 4; // best-effort default when COMP/BINARY without PIC
			case PACKED_DECIMAL -> 0; // cannot infer without PIC
			default -> 0;
			};
		}
		return picture.getByteLength(effective);
	}

	public int calculateOffsets(CopybookNode node, int currentOffset) {
	    node.setStartOffset(currentOffset);

	    if (node instanceof FieldNode field) {
	        int single = calculateByteLength(field.getPicture(), field.getUsage());
	        field.setByteLength(single);
	        node.setByteLength(single);

	        int occurs = Math.max(1, field.getOccursCount());
	        return currentOffset + (single * occurs);
	    }

	    if (node instanceof GroupNode group) {
	        // Identify redefine targets
	        Set<String> redefineTargets = new HashSet<>();
	        for (CopybookNode child : group.getChildren()) {
	            if ((child instanceof GroupNode g && g.isRedefines()) ||
	                (child instanceof FieldNode f && f.isRedefines())) {
	                String target = getRedefinesTarget(child);
	                if (target != null) {
	                    redefineTargets.add(safeName(target));
	                }
	            }
	        }

	        int groupStart = currentOffset;
	        int cursor = groupStart;

	        String currentTarget = null;
	        int redefineSetStart = 0;
	        int maxRedefineLen = 0;

	        for (CopybookNode child : group.getChildren()) {
	            boolean isRedef = (child instanceof GroupNode g && g.isRedefines())
	                          || (child instanceof FieldNode f && f.isRedefines());

	            if (isRedef) {
	                String target = safeName(getRedefinesTarget(child));

	                if (currentTarget != null && !currentTarget.equals(target)) {
	                    cursor = redefineSetStart + maxRedefineLen;
	                    maxRedefineLen = 0;
	                }

	                int end = calculateOffsets(child, redefineSetStart);
	                maxRedefineLen = Math.max(maxRedefineLen, end - redefineSetStart);
	                currentTarget = target;
	                continue;
	            }

	            String childName = safeName(child.getName());
	            boolean isTarget = redefineTargets.contains(childName);

	            if (isTarget) {
	                if (currentTarget != null && maxRedefineLen > 0 && !currentTarget.equals(childName)) {
	                    cursor = redefineSetStart + maxRedefineLen;
	                    maxRedefineLen = 0;
	                }

	                redefineSetStart = cursor;
	                int end = calculateOffsets(child, cursor);
	                maxRedefineLen = Math.max(maxRedefineLen, end - cursor);
	                currentTarget = childName;
	                // do NOT move cursor yet; redefine set decides
	            } else {
	                if (maxRedefineLen > 0) {
	                    cursor = redefineSetStart + maxRedefineLen;
	                    maxRedefineLen = 0;
	                    currentTarget = null;
	                }
	                cursor = calculateOffsets(child, cursor);
	            }
	        }

	        if (maxRedefineLen > 0) {
	            cursor = redefineSetStart + maxRedefineLen;
	        }

	        int singleLen = cursor - groupStart;
	        group.setByteLength(singleLen);
	        node.setByteLength(singleLen);

	        int occurs = Math.max(1, group.getOccursCount());
	        return groupStart + (singleLen * occurs);
	    }

	    return currentOffset;
	}

	
    private String getRedefinesTarget(CopybookNode node) {
        if (node instanceof GroupNode g && g.isRedefines()) {
            return g.getRedefinesTarget();
        } else if (node instanceof FieldNode f && f.isRedefines()) {
            return f.getRedefinesTarget();
        }
        return null;
    }
	
	/**
	 * Total byte length of a FieldNode, including OCCURS.
	 */
	public int calculateByteLength(FieldNode field) {
		if (field == null)
			return 0;

		int single = calculateByteLength(field.getPicture(), field.getUsage());
		int occurs = Math.max(1, field.getOccursCount());
		return single * occurs;
	}

	/**
	 * Total byte length of a GroupNode, including OCCURS, and handling REDEFINES.
	 */
	public int calculateByteLength(GroupNode group) {
		if (group == null)
			return 0;

		int childrenTotal = calculateByteLength(group.getChildren(), 1);
		int occurs = Math.max(1, group.getOccursCount());
		return childrenTotal * occurs;
	}

	/**
	 * Calculates byte length of a list of children, applying REDEFINES rules.
	 *
	 * occursCount is applied once at the end (caller controls whether it should be
	 * applied).
	 */
	public int calculateByteLength(List<CopybookNode> children, int occursCount) {
		if (children == null || children.isEmpty()) {
			return 0;
		}

		// Identify redefine targets (names being redefined)
		Set<String> redefineTargets = new HashSet<>();
		for (CopybookNode child : children) {
			if (child instanceof GroupNode g && g.isRedefines()) {
				redefineTargets.add(safeName(g.getRedefinesTarget()));
			} else if (child instanceof FieldNode f && f.isRedefines()) {
				redefineTargets.add(safeName(f.getRedefinesTarget()));
			}
		}

		int total = 0;

		// Track current redefine set
		int maxRedefineLength = 0;
		String currentTarget = null;

		for (CopybookNode child : children) {
			boolean isRedefine = (child instanceof GroupNode g && g.isRedefines())
					|| (child instanceof FieldNode f && f.isRedefines());

			int childLength = 0;
			if (child instanceof GroupNode g) {
				childLength = calculateByteLength(g); // includes group's OCCURS
			} else if (child instanceof FieldNode f) {
				childLength = calculateByteLength(f); // includes field's OCCURS
			}

			if (isRedefine) {
				String target = (child instanceof GroupNode g) ? safeName(g.getRedefinesTarget())
						: safeName(((FieldNode) child).getRedefinesTarget());

				// If we changed targets, close previous redefine block
				if (currentTarget != null && !currentTarget.equals(target)) {
					total += maxRedefineLength;
					maxRedefineLength = 0;
				}

				currentTarget = target;
				maxRedefineLength = Math.max(maxRedefineLength, childLength);
				continue;
			}

			// Not a redefine entry. Is it the base target of a redefine set?
			String childName = safeName(child.getName());
			boolean isTarget = redefineTargets.contains(childName);

			if (isTarget) {
				// Starting (or continuing) redefine block for this target
				if (currentTarget != null && maxRedefineLength > 0 && !currentTarget.equals(childName)) {
					total += maxRedefineLength;
					maxRedefineLength = 0;
				}
				currentTarget = childName;
				maxRedefineLength = Math.max(maxRedefineLength, childLength);
			} else {
				// Normal entry outside redefine logic
				if (maxRedefineLength > 0) {
					total += maxRedefineLength;
					maxRedefineLength = 0;
					currentTarget = null;
				}
				total += childLength;
			}
		}

		// Close trailing redefine block
		if (maxRedefineLength > 0) {
			total += maxRedefineLength;
		}

		int occurs = Math.max(1, occursCount);
		return total * occurs;
	}

	public int calculateTotalByteLength(GroupNode rootGroup) {
	    return (rootGroup == null) ? 0 : calculateOffsets(rootGroup, 0);
	}

	private static String safeName(String s) {
	    return (s == null) ? "" : s.trim().toUpperCase(Locale.ROOT);
	}

}
