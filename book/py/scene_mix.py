#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (c) 2018 - 2020 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.


# from scene import Scene
from scene_mixin import SceneMixin
from scene_croatian_ties import SceneCroatianTiesMixin
from scene_mayan_ascendancy import SceneMayanAscendancyMixin
from scene_age_of_aquarius import SceneAgeOfAquariusMixin
from scene_mirandas_veil import SceneMirandasVeilMixin
from scene_nineteen import SceneNineteenMixin
from scene_hemeras_dawn import SceneHemerasDawnMixin
from scene_tamoanchan_revisited import SceneTamoanchanRevisitedMixin
from scene_conquest_of_tlalocan import SceneConquestOfTlalocanMixin
from scene_discovery import SceneDiscoveryMixin
from scene_one import SceneOneMixin


class SceneMix(SceneMixin, \
               SceneCroatianTiesMixin, \
               SceneMayanAscendancyMixin, \
               SceneAgeOfAquariusMixin, \
               SceneMirandasVeilMixin, \
               SceneNineteenMixin, \
               SceneHemerasDawnMixin, \
               SceneTamoanchanRevisitedMixin, \
               SceneConquestOfTlalocanMixin, \
               SceneDiscoveryMixin, \
               SceneOneMixin):

    # overrides
    def _get_recent_scene_method_names(self):
        return  [
                    # 'scn_d_09_teleport_wave_via_monolith', \
                    # 'scn_d_10_teleported_wave_blocked', \
                    'scn_o_02_starchild_activating_own_piece_init', \
                    'scn_o_03_starchild_activating_own_piece_end', \
                    # 'scn_o_04_activating_starchild', \
                    # 'scn_o_06_starchild_moving_star_init', \
                    # 'scn_o_07_starchild_moving_star_end', \
                    # 'scn_o_08_starchild_not_moving_monolith_init', \
                    # 'scn_o_09_starchild_not_moving_monolith_end', \
                    # 'scn_o_10_starchild_activated_wave_not_teleporting_init', \
                    # 'scn_o_11_starchild_activated_wave_not_teleporting_end', \
                    # 'scn_o_12_star_moved_wave_teleportation', \
                ]
