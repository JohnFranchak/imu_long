# Check reliability between two coders for a continuously coded pass.

# Parameters
time_threshold = 500 # tolerance for onset and offset differences
pri_col_name = 'position_parent' # name of primary coder's column
rel_col_name = 'position_parent_rel' # name of reliability coder's column
block_col_name = 'rel_blocks' # name of column with reliabilty coding blocks, leave blank if none
codes_to_check = %w(pos) # codes on which to check reliability
disagree_col_name = 'pos_parent_disagree' # name of column to display disagreements, leave blank to skip
clean_col_name = 'pos_parent_clean' # name of column to copy primary coder's column (to make changes after resolving disagreements)
columns_to_show = [block_col_name, pri_col_name, rel_col_name, disagree_col_name, clean_col_name]

## Body
require 'Datavyu_API.rb'

# Fetch all column names
column_names = get_column_list()

# Check to make sure the primary and reliability columns exist (and also id column)
unless ([pri_col_name,rel_col_name] - column_names).empty?
  raise RuntimeError.new('File does not include necessary columns.')
end

# Create mutex column
mutex_col = merge_columns('mutex', pri_col_name, rel_col_name)

# Load columns
col_pri = get_column(pri_col_name)
col_rel = get_column(rel_col_name)

# The prefixes for codes in the mutex column (are names of the source columns)
p_prefix = pri_col_name.downcase
r_prefix = rel_col_name.downcase

# Filter out cells that are outside of the block cells, if any
if block_col_name == ''
  candidate_cells = mutex_col.cells
else
  raise "ERROR: Column #{block_col_name} not found!" unless column_names.include?(block_col_name)

  col_blocks = get_column(block_col_name)
  block_cells = col_blocks.cells
  candidate_cells = mutex_col.cells.select{ |x| block_cells.any?{ |y| y.contains(x) } }
end

# Iterate over the candidate mutex cells and find ones which qualify as disagreements.
intervals = (col_pri.cells + col_rel.cells).map{ |x| [x.onset.to_i, x.offset.to_i] }.uniq
disagreement_cells = candidate_cells.select do |x|
  num_coders = [p_prefix, r_prefix].map{ |y| x.get_code("#{y}_ordinal")}.reject{ |y| y=='' }.size
  flag_dur = x.duration >= time_threshold
  flag_whole = intervals.include?([x.onset.to_i, x.offset.to_i])  # this region is an entire pri or rel cell
  flag_code_differs = codes_to_check.any?{ |y| x.get_code("#{p_prefix}_#{y}") != x.get_code("#{r_prefix}_#{y}") }
  flag_disagree = flag_code_differs && (flag_dur || flag_whole || num_coders==2)
  # printf("%s, %s, %s, %s, %s, %s, %s\n", pri_ord, rel_ord, flag_dur, flag_missing, flag_whole, flag_code_differs, flag_disagree)
  flag_disagree
end

# Accumulate raw disagreement times (from candidate cells)
rdts = Hash[ codes_to_check.collect{ |x| [x, 0] } ]
candidate_cells.each do |x|
  codes_to_check.each do |y|
    pc = x.get_code("#{p_prefix}_#{y}")
    rc = x.get_code("#{r_prefix}_#{y}")
    rdts[y] += x.duration if pc != rc
  end
end

# Accumulate adusted disagreement times (from disagreement_cells)
adts = Hash[ codes_to_check.collect{ |x| [x, 0] } ]
disagreement_cells.each do |x|
  codes_to_check.each do |y|
    pc = x.get_code("#{p_prefix}_#{y}")
    rc = x.get_code("#{r_prefix}_#{y}")
    adts[y] += x.duration if pc != rc
  end
end


# Calculate agreement percentages
total_time = candidate_cells.map(&:duration).reduce(0, :+)
puts "Total time: #{total_time}"
dt_raw = candidate_cells.select do |x|
  codes_to_check.any?{ |y| x.get_code("#{p_prefix}_#{y}") != x.get_code("#{r_prefix}_#{y}") }
end.map(&:duration).reduce(:+)
dt_adj = disagreement_cells.map(&:duration).reduce(:+) # agreement accounting for time_threshold

printf("Raw overall agreement:\t%.2f\%\n", 100.0 * (1.0 - dt_raw.to_f/total_time.to_f))
printf("Adjusted overall agreement:\t%.2f\%\n", 100.0 * (1.0 - dt_adj.to_f/total_time.to_f))

# Print table of raw and adjusted agreements
printf("%-16s|%-16s|%-16s\n", "Code", "Raw agreement %", "Adjusted agreement %")
printf("%-16s+%-16s+%-16s\n", '-'*16, '-'*16, '-'*16)
codes_to_check.each do |code|
  rdt = rdts[code]
  adt = adts[code]

  rdp = 100.0 * (1.0 - rdt.to_f/total_time.to_f)
  adp = 100.0 * (1.0 - adt.to_f/total_time.to_f)

  printf("%-16s|%-16s|%-16s\n", code, rdp.round(2), adp.round(2))
end

# Create and populate disagreements column
unless disagree_col_name == ''
  # Create the disagreements column
  disagree_codes = codes_to_check.map{ |x| "pri_#{x}" } + codes_to_check.map{ |x| "rel_#{x}" } + %w(comments)
  disagree_col = new_column(disagree_col_name, *disagree_codes)

  disagreement_cells.each do |x|
    ncell = disagree_col.new_cell
    codes_to_check.each do |y|
      ncell.change_code("pri_#{y}", x.get_code("#{pri_col_name}_#{y}"))
      ncell.change_code("rel_#{y}", x.get_code("#{rel_col_name}_#{y}"))
      ncell.onset = x.onset
      ncell.offset = x.offset
    end
  end

  # Save the disagreement column
  set_column(disagree_col)
end

# Save pri column as clean column
#set_column(clean_col_name, col_pri)

hide_columns(*get_column_list)
show_columns(*columns_to_show)
puts("Finished.")