
- **MAJOR**: Incompatible API changes  
- **MINOR**: Backward-compatible new features  
- **PATCH**: Backward-compatible bug fixes or minor changes (including placeholders)

---

## Versioning guidelines

| Version   | When to use                                     | Notes                            |
|-----------|-------------------------------------------------|----------------------------------|
| `0.0.0`   | Initial commit, empty or minimal code base      | Baseline version                 |
| `0.0.x`   | Bug fixes, placeholders, minor tweaks           | No new features or API changes   |
| `0.1.0`   | First backward-compatible feature added         | Module or API scaffold present   |
| `0.x.y`   | Continued initial development with features     | Minor increments for features    |
| `1.0.0`   | Stable, public, backward-compatible API         | Production-ready API             |

---

## Notes

- API changes that break backward compatibility require a **MAJOR** version bump.
- Minor and patch numbers should increment sequentially (e.g., `0.0.1`, `0.0.2`, ..., `0.1.0`, `0.1.1`).
- The project is currently in initial development, so `MAJOR` is 0 until API stabilization.

---

## Example

| Commit description             | Version update        |
|--------------------------------|-----------------------|
| Initial commit, license only   | `0.0.0`               |
| Added placeholder functions    | `0.0.1` (patch bump)  |
| Added basic module scaffolding | `0.1.0` (minor bump)  |
| Added retro-compatible feature | `0.2.0` (minor bump)  |
| Bug fix in existing feature    | `0.2.1` (patch bump)  |
| Introduced breaking API change | `1.0.0` (major bump)  |
