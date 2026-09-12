//! The graphical blueprint format: a human-readable text representation that
//! maps onto the current (v0) blueprint.
//!
//! A graphical blueprint string starts with `RAW BLUEPRINT: V0001` followed by
//! a TOML body:
//!
//! ```toml
//! RAW BLUEPRINT: V0001
//! [legend]
//! A = { entity = "assembler1", recipe = "iron_gears" }
//! B = { entity = "assembler1", recipe = "iron_gears", rotation = "East" }
//! P = { entity = "small_power_pole", flipped = { horizontally = true, vertically = false } }
//!
//! [grid]
//! rows = [
//!   "P.AAA",
//!   "..AAA",
//!   "..AAA",
//! ]
//! ```
//!
//! - `[legend]` maps single-character keys to entities. `recipe=` currently
//!   requires an assembler. `rotation=` defaults to [`Rotation::North`] and
//!   `flipped=` (an inline table of `horizontally`/`vertically` booleans) to
//!   [`Flipped::unflipped`].
//! - `[grid].rows` draws one row per string, one character per tile, with `.`
//!   for empty tiles.

use std::collections::HashMap;

use itertools::Itertools;
use serde::Deserialize;

use data::{
    EntityIdentifier,
    entity::{
        GlobalTy,
        assember::{AssemblerTy, Recipe},
        beacon::BeaconTy,
        belt::BeltTy,
        chest::ChestTy,
        extent,
        inserter::InserterTy,
        power_pole::PowerPoleTy,
    },
    spacial::{Extent, Flipped, Position, Rotation},
};

use crate::{
    action::{ActionKind, BuildingInfo, BuildingKind, ForceKind},
    blueprint::string::BlueprintStringCorrupt,
    blueprint::versions::VersionedBlueprint,
};

pub(super) const VERSION: u32 = u32::from_le_bytes(VERSION_BYTES);
pub(super) const VERSION_BYTES: [u8; 4] = *b"0001";

pub(super) struct GraphicalBlueprint {
    actions: Vec<ActionKind>,
}

impl VersionedBlueprint for GraphicalBlueprint {
    fn get_version() -> u32 {
        VERSION
    }
}

impl<'a> TryFrom<&'a [u8]> for GraphicalBlueprint {
    type Error = BlueprintStringCorrupt;

    fn try_from(value: &'a [u8]) -> Result<Self, Self::Error> {
        let text = std::str::from_utf8(value)
            .map_err(|_| missing("graphical blueprint payload is not UTF-8"))?;

        parse(text)
    }
}

impl From<GraphicalBlueprint> for super::Blueprint {
    fn from(value: GraphicalBlueprint) -> Self {
        Self {
            actions: value.actions,
        }
    }
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct BlueprintFile {
    legend: HashMap<String, EntitySpec>,
    grid: GridSpec,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct EntitySpec {
    entity: String,
    recipe: Option<String>,
    rotation: Option<Rotation>,
    flipped: Option<Flipped>,
}

#[derive(Default, Deserialize)]
#[serde(deny_unknown_fields)]
struct GridSpec {
    rows: Vec<String>,
}

fn missing(message: impl Into<String>) -> BlueprintStringCorrupt {
    BlueprintStringCorrupt::MissingThing(message.into())
}

fn parse(text: &str) -> Result<GraphicalBlueprint, BlueprintStringCorrupt> {
    let file: BlueprintFile = toml::from_str(text)
        .map_err(|error| missing(format!("cannot parse the blueprint: {error}")))?;

    let legend = build_legend(file.legend)?;
    let grid: Vec<Vec<char>> = file
        .grid
        .rows
        .into_iter()
        .map(|row| row.chars().collect())
        .collect();

    Ok(GraphicalBlueprint {
        actions: place_all(&grid, &legend)?,
    })
}

/// Places one entity per legend character, anchored with its top-left corner
/// at that character's cell. Each legend entry carries the entity's kind plus
/// its rotation and flippage, with only the position left to fill in.
fn place_all(
    grid: &[Vec<char>],
    legend: &HashMap<char, BuildingInfo>,
) -> Result<Vec<ActionKind>, BlueprintStringCorrupt> {
    let width = grid.first().map_or(0, Vec::len);
    if grid.iter().any(|row| row.len() != width) {
        return Err(missing("all grid rows must have the same length"));
    }

    let mut seen = vec![vec![false; width]; grid.len()];
    let mut actions = Vec::new();

    for (y, row) in grid.iter().enumerate() {
        for (x, &ch) in row.iter().enumerate() {
            if ch == '.' || seen[y][x] {
                continue;
            }

            let entry = legend
                .get(&ch)
                .ok_or_else(|| missing(format!("grid character {ch:?} has no legend entry")))?;

            let size = extent(entry.kind.get_global_ty(), entry.rotation, entry.flipped);
            verify_footprint(grid, &mut seen, x, y, ch, size)?;

            let mut building_info = entry.clone();
            building_info.position = Position {
                x: i32::try_from(x).map_err(|_| missing("the grid is too large"))?,
                y: i32::try_from(y).map_err(|_| missing("the grid is too large"))?,
            };
            actions.push(ActionKind::PlaceBuilding {
                ghost: false,
                force: ForceKind::None,
                building_info,
            });
        }
    }

    Ok(actions)
}

/// Checks that the entity bounding box starting at (`x`, `y`) is fully drawn
/// with `ch` and marks those cells as seen, rejecting footprints that leave
/// the grid or are only partially drawn.
fn verify_footprint(
    grid: &[Vec<char>],
    seen: &mut [Vec<bool>],
    x: usize,
    y: usize,
    ch: char,
    size: Extent,
) -> Result<(), BlueprintStringCorrupt> {
    let width = grid.first().map_or(0, Vec::len);
    let (w, h) = (size.width as usize, size.height as usize);

    if x + w > width || y + h > grid.len() {
        return Err(missing(format!(
            "the entity at ({x}, {y}) extends past the edge of the grid"
        )));
    }

    for (seen_row, grid_row) in seen[y..y + h].iter_mut().zip(&grid[y..y + h]) {
        for (seen_cell, cell) in seen_row[x..x + w].iter_mut().zip(&grid_row[x..x + w]) {
            if *cell != ch {
                return Err(missing(format!(
                    "the entity at ({x}, {y}) must be drawn as a full {w}x{h} block of {ch:?}"
                )));
            }
            *seen_cell = true;
        }
    }

    Ok(())
}

fn build_legend(
    legend: HashMap<String, EntitySpec>,
) -> Result<HashMap<char, BuildingInfo>, BlueprintStringCorrupt> {
    let mut built = HashMap::with_capacity(legend.len());

    for (key, spec) in legend {
        let Some([ch]) = key.chars().collect_array() else {
            return Err(missing(format!(
                "legend key {key:?} must be a single character"
            )));
        };

        let recipe = spec
            .recipe
            .as_deref()
            .map(|value| {
                Recipe::try_from(value).map_err(|()| missing(format!("illegal recipe {value:?}")))
            })
            .transpose()?;

        let id = if spec.entity.contains("::") {
            spec.entity.clone()
        } else {
            format!("factory_game::{}", spec.entity)
        };

        let global_ty = GlobalTy::try_from(EntityIdentifier::new_raw(id))
            .map_err(|_| missing(format!("unknown entity {:?}", spec.entity)))?;

        let kind = building_kind_for(global_ty, recipe, &spec.entity)?;

        built.insert(
            ch,
            BuildingInfo {
                // Overwritten per placement by `place_all`.
                position: Position { x: 0, y: 0 },
                rotation: spec.rotation.unwrap_or(Rotation::North),
                flipped: spec.flipped.unwrap_or_else(Flipped::unflipped),
                kind,
            },
        );
    }

    Ok(built)
}

fn building_kind_for(
    global_ty: GlobalTy,
    recipe: Option<Recipe>,
    id: &str,
) -> Result<BuildingKind, BlueprintStringCorrupt> {
    if let Ok(ty) = AssemblerTy::try_from(global_ty) {
        return Ok(BuildingKind::Assembler {
            ty,
            recipe,
            modules: vec![],
        });
    }

    if recipe.is_some() {
        return Err(missing(format!(
            "the 'recipe' attribute requires an assembler, but {id:?} is not one"
        )));
    }

    if let Ok(ty) = InserterTy::try_from(global_ty) {
        return Ok(BuildingKind::Inserter { ty });
    }

    if let Ok(ty) = ChestTy::try_from(global_ty) {
        return Ok(BuildingKind::Chest { ty });
    }

    if let Ok(ty) = PowerPoleTy::try_from(global_ty) {
        return Ok(BuildingKind::PowerPole { ty });
    }

    if let Ok(ty) = BeltTy::try_from(global_ty) {
        return Ok(BuildingKind::Belt { ty });
    }

    if let Ok(ty) = BeaconTy::try_from(global_ty) {
        return Ok(BuildingKind::Beacon { ty });
    }

    Err(missing(format!(
        "entity {id:?} is not placeable via graphical blueprints"
    )))
}
