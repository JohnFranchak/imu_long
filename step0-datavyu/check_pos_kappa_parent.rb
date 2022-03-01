# Compute kappa score for ----- coding.

## Parameters
primary_column_name = 'position_parent'
reliability_column_name = 'position_parent_rel'
blocks_columns = %w[rel_blocks]
codes_to_check = %w[pos]
blank_value = ''
default_step_size = 34

## Body
require 'Datavyu_API.rb'

def dv_step_size
  fps = Datavyu.get_video_controller.get_frame_rate_controller.get_frame_rate
  if fps.zero?
    nil
  else
    (1000.0 / fps).ceil
  end
end

def contingency_table(pri_values, rel_values)
  pri_values.flatten!
  rel_values.flatten!
  return [nil, {}] if pri_values.empty? || rel_values.empty?

  # Build a hashmap from the list of codes to all observed values for that code
  # across primary and reliability cells.
  observed_values = (pri_values + rel_values).uniq

  # Init contingency tables for each code name
  table = CTable.new(*observed_values)

  # Fill the contingency table.
  pri_values.zip(rel_values).each do |pv, rv|
    table.add(pv, rv)
  end
  table
end

pri_col = get_column(primary_column_name)
rel_col = get_column(reliability_column_name)

# Blocks cells are nested cells inside all blocks columns.
# Create a dummy block cell if no blocks columns specified.
if blocks_columns.empty?
  bcol = new_column('dummy_blocks', 'dummy_code')
  ncell = bcol.new_cell
  ncell.onset = (pri_col.cells + rel_col.cells).map(&:onset).min
  ncell.offset = (pri_col.cells + rel_col.cells).map(&:offset).max
  blocks_cells = [ncell]
else
  blocks_cols = blocks_columns.map { |cname| get_column(cname) }
  blocks_cells = blocks_cols.last.cells
  blocks_cols[0..-2].each do |outer_col|
    blocks_cells.select! do |bc|
      outer_col.cells.any? { |ocell| ocell.contains(bc) }
    end
  end
end

step_size = dv_step_size || default_step_size
puts "Using step size: #{step_size}"

p_vals = Hash.new([])
r_vals = Hash.new([])
blocks_cells.each do |bc|
  puts "Getting codes from block #{bc.ordinal}..."
  time = bc.onset
  while time < bc.offset
    interval = time...time + step_size
    p_cell = pri_col.cells.find { |pc| pc.overlaps_range(interval) }
    r_cell = rel_col.cells.find { |rc| rc.overlaps_range(interval) }

    codes_to_check.each do |code|
      p_vals[code] << (p_cell.nil? ? blank_value : p_cell.get_code(code))
      r_vals[code] << (r_cell.nil? ? blank_value : r_cell.get_code(code))
    end

    time += step_size
  end
end

tables = codes_to_check.map do |code|
  contingency_table(p_vals[code], r_vals[code])
end

tables.each do |t|
  printf("Kappa score:\t%.2f\n", t.kappa)
  puts t
  puts
end
